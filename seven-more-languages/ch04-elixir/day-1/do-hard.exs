defmodule Day1Hard do
  # create indentation based on level context
  def make_indent(level), do: String.duplicate("  ", level)

  def traverse_with_level(data, level, mk_indent) do
    if is_tuple(data) do
      Enum.each(
        Tuple.to_list(data), 
        fn (x) -> traverse_with_level(x, level+1, mk_indent) end)
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
  # * otherwise we examine empty cells, take the one that has best "potential"
  # * to estimate "potential", we count lines that might lead to a win (i.e. not
  #   yet taken by the enemy) (note that if the whole board is empty, the center cell
  #   will have a potential of 4, the corners 3, and sides 2, leaving the center cell
  #   the strongest candidate, which is what we want and how a game usually begins)

  # returns :x, :o or nil indicating who does the next move
  # nil if the board is already full
  def board_next_move( { {a,b,c}, {d,e,f}, {g,h,i} } ) do
    xs = [a,b,c,d,e,f,g,h,i]
    if Enum.all?(xs, fn (x) -> not is_nil(x) end) do
      nil
    else
      "test"
    end
  end
end

data = {"See Spot.", {"See Spot sit.", "See Spot run.", {"Go Deeper"}, "Back one level"}}

# we will have an extra indentation on top level, which I think is resonable:
# consider the difference between passing "foo" and parsing {"foo"},
# we want their representations to be unique.
Day1Hard.traverse( data )

p = fn (x) -> IO.puts (inspect x) end
p.( Day1Hard.board_next_move( {{:x,:o,:x}, {:o,:x,:o}, {:x,:o,:x}} ) )
