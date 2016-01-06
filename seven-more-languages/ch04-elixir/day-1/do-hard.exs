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

  def detect_winning_board_move( board, p ) do
    Enum.reduce(
      line_table,
      # no winning move is found
      false,
      fn ( line = {pos1, pos2, pos3}, acc ) ->
        case acc do
          false ->
            access = fn ({x,y}, board) -> elem(elem(board,x),y) end
            result = detect_winning_line_move(
              { access.(pos1,board),
                access.(pos2,board),
                access.(pos3,board) },
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

  def best_next_move(board) do
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
        # TODO: refine
        "no obvious solution"
      end
    end
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
