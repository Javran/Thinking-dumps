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
end

data = {"See Spot.", {"See Spot sit.", "See Spot run.", {"Go Deeper"}, "Back one level"}}

# we will have an extra indentation on top level, which I think is resonable:
# consider the difference between passing "foo" and parsing {"foo"},
# we want their representations to be unique.
Day1Hard.traverse( data )
