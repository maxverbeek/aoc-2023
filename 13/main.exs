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

  def slice_around(block, point) do
      block_size = length(block)
      rlen = block_size - point

      len = min(point, rlen)

      lrange = (point - len)..(point - 1)
      rrange = (point)..(point+len - 1)

      left = block |> Enum.slice(lrange)
      right = block |> Enum.slice(rrange) |> Enum.reverse

      { :ok, left, right }
  end

  def find_reflection(block) do
    block_size = length(block)
    Enum.find(1..(block_size - 1), fn point ->
      {:ok, left, right} = slice_around(block, point)

      Enum.zip(left, right) |> Enum.all?(&elem(&1, 0) == elem(&1, 1))
    end)
  end

  def smudges(block) do
    block_size = length(block)
    Enum.find(1..(block_size - 1), fn point ->
      {:ok, left, right} = slice_around(block, point)

      left = Enum.concat(left)
      right = Enum.concat(right)

      width = length(left)

      # in the smudged mirror, there must be exactly 1 inequality
      equalities = Enum.zip(left, right) |> Enum.map(&elem(&1, 0) == elem(&1, 1))

      # Over engineered: there is only 1 smudge, no need for intersection between the transposed smudges.
      # Just returning the index at which there is only 1 mutation is sufficient
      # case Enum.count(equalities, fn b -> b == false end) do
      #   1 ->
      #     pos = Enum.find_index(equalities, fn b -> b == false end)
      #     row = div(pos, width)
      #     col = rem(pos, width)
      #     { :smudge, row: row, col: col }
      #   0 -> { :clean }
      #   _ -> { :dirty }
      # end

      Enum.count(equalities, fn b -> b == false end) == 1
    end)
  end

  def select_smudge(res) do
    smudges = Enum.with_index(res, fn e, i -> { e, i } end)
      |> Enum.filter(fn { el, _idx } -> { :smudge } == el end)
      |> Enum.map(fn { _el, idx } -> idx end)

    IO.inspect(smudges)
    
    case smudges do
      [idx] -> idx + 1
      [] -> nil
      _ -> raise "more than 1 smudge"
    end
  end

  def part1(input) do
    horizontal = Enum.map(input, &AOC.find_reflection/1)

    transposed = Enum.map(input, &AOC.transpose/1)
    vertical = Enum.map(transposed, &AOC.find_reflection/1)

    hsum = Enum.filter(horizontal, fn x -> x != nil end) |> Enum.map(fn x -> x * 100 end) |> Enum.sum
    vsum = Enum.filter(vertical, fn x -> x != nil end) |> Enum.sum

    hsum + vsum
  end


  def part2(input) do
    horizontal = Enum.map(input, &AOC.smudges/1)
    vertical = Enum.map(input, &transpose/1) |> Enum.map(&smudges/1)

    hsum = Enum.filter(horizontal, fn x -> x != nil end) |> Enum.map(&(&1 * 100)) |> Enum.sum()
    vsum = Enum.filter(vertical, fn x -> x != nil end) |> Enum.sum()

    hsum + vsum
  end
end

input = AOC.read_lines()
IO.inspect(AOC.part1(input))
IO.inspect(AOC.part2(input))
