defmodule Day1Medium do

  def list_length([]), do: 0
  def list_length([_|tl]), do: 1+list_length(tl)

  def fold_left(_, seed, []), do: seed
  def fold_left(g, seed, [hd|tl]), do: fold_left(g, g.(seed,hd), tl)

  def find_max([hd|tl]), do: fold_left( &max/2, hd, tl )
  def find_min([hd|tl]), do: fold_left( &min/2, hd, tl )

end

ls1 = []
ls2 = [1,2,3]
ls3 = [1468,2560,897,3610,3713,2348]

p = fn (x) -> IO.puts (inspect x) end

p.( Day1Medium.list_length(ls1))
p.( Day1Medium.list_length(ls2))
p.( Day1Medium.list_length(ls3))

p.( Day1Medium.find_max(ls2) )
p.( Day1Medium.find_max(ls3) )

p.( Day1Medium.find_min(ls2) )
p.( Day1Medium.find_min(ls3) )
