defmodule AOC do

  def read_lines(acc \\ []) do
    case IO.read(:stdio, :line) do
      "\n" -> [Enum.reverse(acc) | read_lines([])]
      :eof ->
        [Enum.reverse(acc)]
      line ->
        charlist = line |> String.trim_trailing |> String.graphemes
        read_lines([charlist | acc])
    end
  end

  def transpose(rows) do
    rows
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
  end

  def find_reflection(block) do
    block_size = length(block)
    Enum.find(1..(block_size - 1), fn point ->
      rlen = block_size - point

      len = min(point, rlen)

      lrange = (point - len)..(point - 1)
      rrange = (point)..(point+len - 1)

      left = block |> Enum.slice(lrange)
      right = block |> Enum.slice(rrange) |> Enum.reverse

      # IO.inspect(lrange)
      # IO.inspect(rrange)
      # IO.inspect(left)
      # IO.inspect(right)

      Enum.zip(left, right) |> Enum.all?(&elem(&1, 0) == elem(&1, 1))
    end)
  end

  def part1(input) do
    horizontal = Enum.map(input, &AOC.find_reflection/1)

    transposed = Enum.map(input, &AOC.transpose/1)
    vertical = Enum.map(transposed, &AOC.find_reflection/1)

    hsum = Enum.filter(horizontal, fn x -> x != nil end) |> Enum.map(fn x -> x * 100 end) |> Enum.sum
    vsum = Enum.filter(vertical, fn x -> x != nil end) |> Enum.sum

    hsum + vsum
  end
end

input = AOC.read_lines()
IO.inspect(AOC.part1(input))
