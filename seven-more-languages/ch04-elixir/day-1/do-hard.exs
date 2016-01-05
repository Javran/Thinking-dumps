defmodule Day1Hard do
  # create indentation based on level context
  def make_indent(level), do: String.duplicate("  ", level)

  def traverse_with_level(data, level, mk_indent) do
    if is_tuple(data) do
      xs = Tuple.to_list(data)
      Enum.each(xs, fn (x) -> traverse_with_level(x, level+1, mk_indent) end)
    else
      IO.puts (mk_indent.(level) <> data)
    end
  end

  def traverse(data), do: traverse_with_level(data,0,&make_indent/1)

  # tic-tac-toe best move:
  # we will represent a board as a tuple: { {a,b,c}, {d,e,f}, {g,h,i} }
  # all variables are one of: nil, :x, :o
  # * first determine who is doing the next move (by counting cells taken)
  # * if there's an obvious way to win, do it
  # * if enemy has an obvious way of winning, block it
  # * otherwise we examine empty cells, take the one that has best "potential"
  # * to estimate "potential", we count lines that might lead to a win (i.e. not
  #   yet taken by the enemy) (note that if the whole board is empty, the center cell
  #   will have a potential of 4, the corners 3, and sides 2, leaving the center cell
  #   the strongest candidate, which is what we want and how a game usually begins)
end

data = {"See Spot.", {"See Spot sit.", "See Spot run.", {"Go Deeper"}, "Back one level"}}

# we will have an extra indentation on top level, which I think is resonable:
# consider the difference between passing "foo" and parsing {"foo"},
# we want their representations to be unique.
Day1Hard.traverse( data )
