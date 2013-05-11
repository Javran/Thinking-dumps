// note this file only works in interactive mode

open System
open System.IO
open System.Collections.Generic

type MultiFileLogger() =
    do printfn "Constructing ..."

    let m_logs = new List<StreamWriter>()

    member this.AttachLogFile file =
        let newLogFile = new StreamWriter(file,true)
        m_logs.Add(newLogFile)

    member this.LogMessage (msg:string) =
        m_logs |> Seq.iter (fun writer -> writer.WriteLine(msg))

    interface IDisposable with
        member this.Dispose() =
            printfn "Cleaning up ..."
            m_logs |> Seq.iter (fun writer -> writer.Dispose())
            m_logs.Clear()

let task1 () =
    use logger = new MultiFileLogger()
    logger.AttachLogFile "1.log"
    logger.AttachLogFile "2.log"
    logger.LogMessage "2 files are opened for logging."
    logger.LogMessage "Quiting ..."
    ();;

task1 ();;
// Will see message: "Constructing ..." and "Quiting ..."

#quit;;
