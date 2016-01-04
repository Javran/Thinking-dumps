defmodule Day1Medium do

  def list_length([]), do: 0
  def list_length([_|tl]), do: 1+list_length(tl)

  def find_max_aux(cur_max,[]), do: cur_max

  def find_max_aux(cur_max,[hd|tl]) do
    if cur_max < hd do
      find_max_aux(hd,tl)
    else
      find_max_aux(cur_max,tl)
    end
  end

  def find_max([hd|tl]), do: find_max_aux(hd,tl)

end

ls1 = []
ls2 = [1,2,3]
ls3 = [1468,2560,897,3610,3713,2348]

IO.puts (inspect Day1Medium.list_length(ls1))
IO.puts (inspect Day1Medium.list_length(ls2))
IO.puts (inspect Day1Medium.list_length(ls3))

IO.puts (inspect Day1Medium.find_max(ls2))
IO.puts (inspect Day1Medium.find_max(ls3))
