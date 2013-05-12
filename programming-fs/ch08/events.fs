// note this file only works in interactive mode

// Events:
// "just syntactic sugar for properties on classes that are delegates"
// Will see in practice

// creating events

// idiomatic way to declare events is to have the delegate return unit
//      and take two parameters: source & a class derived from EventArgs

type SetAction = Added | Removed

type SetOperationEventArgs<'a> (value : 'a, action : SetAction) =
    inherit System.EventArgs()

    member this.Action = action
    member this.Value = value

type SetOperationDelegate<'a> = 
    delegate of obj * SetOperationEventArgs<'a> -> unit


type NoisySet<'a when 'a : comparison>() =
    let mutable m_set = Set.empty : Set<'a>

    let m_itemAdded =
        new Event<SetOperationDelegate<'a>, SetOperationEventArgs<'a>>()

    let m_itemRemoved =
        new Event<SetOperationDelegate<'a>, SetOperationEventArgs<'a>>()

    member this.Add(x) =
        m_set <- m_set.Add(x)
        m_itemAdded.Trigger(this, new SetOperationEventArgs<_>(x, Added))

    member this.Remove(x) =
        m_set <- m_set.Remove(x)
        m_itemRemoved.Trigger(this, new SetOperationEventArgs<_>(x, Removed))

    member this.ItemAddedEvent = m_itemAdded.Publish
    member this.ItemRemovedEvent = m_itemRemoved.Publish

let s = new NoisySet<int>()

let setOperationHandler =
    new SetOperationDelegate<int>(
        fun sender args ->
            printfn "%d was %A" args.Value args.Action)

s.ItemAddedEvent.AddHandler(setOperationHandler)
s.ItemRemovedEvent.AddHandler(setOperationHandler);;

s.Add(9);;
s.Remove(9);;

s.ItemAddedEvent.RemoveHandler(setOperationHandler)
s.ItemRemovedEvent.RemoveHandler(setOperationHandler);;

// we can no longer receive message because we've unsubscribed then
s.Add(9);;
s.Remove(9);;

// the Event<_,_> class

open System

type ClockUpdateDelegate = delegate of int * int * int -> unit

type Clock () =
    // the corresponding event
    let m_event = new DelegateEvent<ClockUpdateDelegate>()

    // start and trigger the event for limited times
    member this.Start(time) =
        printfn "Started ..."
        for i = 1 to time do
            Threading.Thread.Sleep(100)

            let hour = DateTime.Now.Hour
            let minute = DateTime.Now.Minute
            let second = DateTime.Now.Second

            // seems the delegate will be invoked with arguments passed here.
            m_event.Trigger( [| box hour; box minute; box second |] )

    member this.ClockUpdate =
        m_event.Publish

let c = new Clock()

c.ClockUpdate.AddHandler(
    new ClockUpdateDelegate(
        fun h m s -> printfn "[%02d:%02d:%02d]" h m s));;

// trigger for 10 times
// uncomment the line below to see the example
//c.Start(10);;

// the observable module

type MusicGenre = Classical | Pop | HipHop | Rock | Latin | Country

type Song =
    { Title : string
    ; Genre : MusicGenre
    ; BPM : int }

// but why just simply pass type Song in ?
type SongChangeArgs(song : Song) =
    inherit System.EventArgs()

    member this.Song = song

type SongChangeDelegate = delegate of obj * SongChangeArgs -> unit

type JukeBox() =
    let m_songStartedEvent = new Event<SongChangeDelegate, SongChangeArgs>()
    
    member this.PlaySong(song) =
        m_songStartedEvent.Trigger(
            this,
            new SongChangeArgs(song))
    
    [<CLIEvent>]
    member this.SongStartedEvent =
        m_songStartedEvent.Publish

let jb = new JukeBox()

let fastSongEvent, slowSongEvent =
    jb.SongStartedEvent
        |> Observable.filter( fun songArgs ->
            match songArgs.Song.Genre with
            | Pop | HipHop | Latin | Country -> true
            | _ -> false)
        |> Observable.partition(fun songArgs ->
            songArgs.Song.BPM >= 120);;

slowSongEvent.Add(fun args ->
    printfn "Slow: %s" args.Song.Title)

fastSongEvent.Add(fun args ->
    printfn "Fast: %s" args.Song.Title)

jb.PlaySong( { Title = "Burin Love"; Genre = Pop; BPM = 120 } );;
jb.PlaySong( { Title = "Filtered"; Genre = Classical; BPM = 120 } );;
jb.PlaySong( { Title = "Slow Song"; Genre = Country; BPM = 60 } );;

open System.Windows.Forms

let testObservableAdd () =
    let form = new Form(Text="Keep out of the bottom!", TopMost=true)
    
    form.MouseMove
    |> Observable.filter (fun moveArgs -> moveArgs.Y > form.Height / 2)
    |> Observable.add    (fun moveArgs -> MessageBox.Show("Moved into bottom half.")
                                            |> ignore)

    form.ShowDialog() |> ignore

// uncomment the line below to see the example
//testObservableAdd();;

// takes two input event, will be fired whether either of its input event is raised

let justDanceEvent = Observable.merge slowSongEvent fastSongEvent
justDanceEvent.Add( fun args -> printfn "Either slowSongEvent or fastSongEvent is raised" )

jb.PlaySong(
    { Title = "Escape (The Pina Colada Song)"; Genre = Pop; BPM = 70 } );;

let testObservableMap () =
    let form = new Form(Text="Relative Clicking", TopMost=true)

    form.MouseClick.AddHandler(
        new MouseEventHandler(
            fun sender clickArgs ->
                printfn "MouseClickEvent, @ [%d, %d]" clickArgs.X clickArgs.Y))

    let centeredClickEvent =
        form.MouseClick
        |> Observable.map (fun clickArgs ->
            clickArgs.X - (form.Width / 2),
            clickArgs.Y - (form.Height / 2))

    centeredClickEvent
    |> Observable.add( fun (x,y) -> printfn "CenteredClickEvent, @ [%d, %d]" x y)

    form.ShowDialog() |> ignore

// uncomment the line below to see the example
//testObservableMap();;

// creating .Net events

// use attribute "[<CLIEvent>]" to make the Event compatible with other .Net languages

#quit;;
