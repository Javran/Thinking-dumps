defmodule Day1Hard do
  # create indentation based on level context
  def make_indent(level), do: String.duplicate("  ", level)

  def traverse_with_level(data, level, mk_indent) do
    if is_tuple(data) do
      Enum.each(
        Tuple.to_list(data),
        &(traverse_with_level(&1, level+1, mk_indent)))
    else
      IO.puts (mk_indent.(level) <> data)
    end
  end

  def traverse(data), do: traverse_with_level(data,0,&make_indent/1)

  # tic-tac-toe best move:
  # we will represent a board as a tuple: { {a,b,c}, {d,e,f}, {g,h,i} }
  # all variables are one of: nil, :x, :o
  # * first determine who is doing the next move (by counting cells taken)
  #   assume :x moves first when the number of cells taken is the same for both
  # * if there's an obvious way to win, do it
  # * if enemy has an obvious way of winning, block it
  # * otherwise we examine empty cells, take the one that has best "potential" (to the foe)
  # * to estimate "potential", we count lines that might lead to a win (i.e. not
  #   yet taken by the enemy) (note that if the whole board is empty, the center cell
  #   will have a potential of 4, the corners 3, and sides 2, leaving the center cell
  #   the strongest candidate, which is what we want and how a game usually begins)

  # returns :x, :o or nil indicating who does the next move
  # nil if the board is already full
  def board_next_player( { {a,b,c}, {d,e,f}, {g,h,i} } ) do
    # TODO: separate board-flatten logic to another function
    # (and use Tuple.to_list to do so)
    xs = [a,b,c,d,e,f,g,h,i]
    if Enum.all?(xs, &(not is_nil(&1))) do
      nil
    else
      {x_count,o_count} = 
        Enum.reduce(
          xs, 
          {0,0},
          fn (i,{xc,oc}) ->
            case i do
              :x -> {xc+1,oc}
              :o -> {xc,oc+1}
              _  -> {xc,oc}
            end
          end)
      if x_count <= o_count do
        :x
      else 
        :o
      end
    end
  end

  # given a line and a player, return the position that leads to a win (on this line)
  # if there's no direct winning move, return false
  def detect_winning_line_move( line, p ) do
    case line do
      # IMPORTANT: pin the pattern!
      {nil,^p,^p} -> 0
      {^p,nil,^p} -> 1
      {^p,^p,nil} -> 2
      _ -> false
    end
  end

  # hm, we could generate this table, but
  # that sounds like an overkill
  def line_table do
    [ # rows
      { {0,0}, {0,1}, {0,2} },
      { {1,0}, {1,1}, {1,2} },
      { {2,0}, {2,1}, {2,2} },
      # cols
      { {0,0}, {1,0}, {2,0} },
      { {0,1}, {1,1}, {2,1} },
      { {0,2}, {1,2}, {2,2} },
      # diags
      { {0,0}, {1,1}, {2,2} },
      { {0,2}, {1,1}, {2,0} } ]
  end

  def access({x,y}, board), do: elem(elem(board,x),y)

  def detect_winning_board_move( board, p ) do
    Enum.reduce(
      line_table,
      # no winning move is found
      false,
      fn ( line = {pos1, pos2, pos3}, acc ) ->
        case acc do
          false ->
            result = detect_winning_line_move(
              { access(pos1,board),
                access(pos2,board),
                access(pos3,board) },
              p)
            case result do
              false -> false
              idx -> elem(line, idx)
            end
          _ -> acc
        end
      end)
  end
  
  def another_player p do
    case p do
      :x -> :o
      :o -> :x
    end
  end

  # update a 3-tuple in the specified location
  def tuple3_modify({a,b,c}, idx, modifier) do
    case idx do
      0 -> {modifier.(a),b,c}
      1 -> {a,modifier.(b),c}
      2 -> {a,b,modifier.(c)}
    end
  end

  # return a score in board's shape
  # all cells in that line gets one score
  # if all of them are either empty or taken by that player
  # e.g. { {:x, nil, nil}, {nil,nil,nil}, {nil,nil,nil} }
  # with line { {0,0}, {0,1}, {0,2} } is:
  # { {1,1,1}, {0,0,0}, {0,0,0} } to :x
  # but all zero to :o
  def score_line(board,{p1,p2,p3},player) do
    foe = another_player(player)
    cells = 
      {access(p1,board),
       access(p2,board),
       access(p3,board)}
    score = 
      case cells do
        {^foe,_,_} -> 0
        {_,^foe,_} -> 0
        {_,_,^foe} -> 0
        _ -> 1
      end
    Enum.reduce(
      [p1,p2,p3],
      {{0,0,0},{0,0,0},{0,0,0}},
      fn ({r_index,c_index}, score_board) ->
        tuple3_modify(
          score_board,
          r_index,
          fn (row) ->
            tuple3_modify(
              row,
              c_index,
              &( &1 + score ))
          end)
      end)
  end

  def merge_score({l1,l2,l3},{r1,r2,r3}) do
    merge_line =
      fn ({a,b,c}, {d,e,f}) -> {a+d,b+e,c+f} end
    { merge_line.(l1,r1),
      merge_line.(l2,r2),
      merge_line.(l3,r3) }
  end

  def score_board(board, player) do
    Enum.reduce(
      line_table,
      {{0,0,0},{0,0,0},{0,0,0}},
      fn (line, acc) ->
        merge_score(acc, score_line(board,line,player))
      end)
  end

  # return one of: :x, :o, :tie
  # or false if the game is not finished
  def get_winner(board) do
    winner = Enum.reduce(
      line_table,
      # nil for not decided
      nil,
      fn ({p1,p2,p3}, acc) ->
        case acc do
          nil ->
          case {access(p1,board),
                access(p2,board),
                access(p3,board)} do
            {a,a,a} -> a
            _ -> nil
          end
          _ -> acc
        end
      end)
    case winner do
      nil -> 
        case board_next_player(board) do
          nil -> :tie
          _ -> false
        end
      _ -> winner
    end
  end

  # this function works only on incomplete boards
  def best_next_move_incomplete(board) do
    player = board_next_player(board)
    foe = another_player(player)
    win_move = detect_winning_board_move(board, player)
    if win_move do
      win_move
    else
      other_win_move = detect_winning_board_move(board, foe)
      if other_win_move do
        other_win_move
      else
        # analyze the best move for the foe
        score = score_board(board, foe)
        # all possible coordinates, let's take it from existing line_table
        coordinates =
          Enum.flat_map(
            Enum.take(Day1Hard.line_table,3),
            &Tuple.to_list/1)
        candiates =
          Enum.sort_by(
            coordinates,
            fn (pos) ->
              case access(pos,board) do
                nil -> access(pos,score)
                _ -> 
                  # here we have a score lower than 0
                  # so that unless the board is fully filled,
                  # we cannot choose a taken cell as next best move
                  -1
              end
            end,
            &( &1 >= &2 ))
        [solution|_] = candiates
        solution
      end
    end
  end

  def best_next_move(board) do
    case get_winner(board) do
      false ->
        # only deal with incomplete baord
        best_next_move_incomplete(board)
      _ -> 
        # if we have reached an end state, there's nothing to do
        false
    end
  end

  # TODO: play with itself and pretty print

  # print a board to stdout
  def print_board {l1,l2,l3} do
    cell_str = fn (x) ->
      case x do
        :x -> "X"
        :o -> "O"
        nil -> " "
      end
    end
    line_str = fn {x,y,z} ->
      cell_str.(x) <> "|" <> cell_str.(y) <> "|" <> cell_str.(z)
    end
    sep = "-+-+-"
    IO.puts( line_str.(l1) )
    IO.puts sep
    IO.puts( line_str.(l2) )
    IO.puts sep
    IO.puts( line_str.(l3) )
  end 
  
end

data = {"See Spot.",
        {"See Spot sit.",
         "See Spot run.",
         {"Go Deeper"},
         "Back one level"}}

# we will have an extra indentation on top level, which I think is resonable:
# consider the difference between passing "foo" and parsing {"foo"},
# we want their representations to be unique.
Day1Hard.traverse( data )

# p = fn (x) -> IO.puts (inspect x) end

Day1Hard.print_board( {  {:x,nil,:o}, {:o,:x,:x}, {nil,:o,nil} } )
