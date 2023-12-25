graph = {}

STDIN.read.split("\n").each do |line|
  source, dest = line.split(': ')
  dest = dest.split(' ')

  destinations = graph[source] || []
  graph[source] = destinations.concat(dest)

  dest.each do |d|
    graph[d] = (graph[d] || []) + [source]
  end
end

def bfs(graph, start, stop)

  path = {start => start}
  queue = [start]

  while queue.size > 0
    node = queue.shift

    if node == stop then
      # make path list
      visited = []
      while node != path[node]
        visited.push node
        node = path[node]
      end

      return visited.reverse
    end

    neighbours = graph[node].reject { |x| path.include? x }
    
    neighbours.each do |n|
      path[n] = node
    end

    queue.concat neighbours
  end

  puts "no path found between #{start} and #{stop} :("

  return []
end

# sample a bunch of nodes randomly, and see which are uesd the most

histogram = {}

4000.times do |i|
  start = graph.keys.sample
  stop = graph.keys.sample

  visited = bfs(graph, start, stop)

  visited.each do |v|
    histogram[v] = (histogram[v] || 0) + 1
  end
end

frequent = histogram.sort_by { |_k, v| -v }

# of the top 3 connections (6 connections because they are bidirectional),
# remove the edge
top6 = frequent.first(6).to_h.keys

top6.each do |key|
  graph[key] = graph[key].reject { |dst| top6.include? dst }
end

# apply union find disjoint set algo to determine the connected components. in
# this algo, let the parent be the smaller node by the < operator

# 1. initially each node is their own parent
parents = {}

graph.keys.each do |k|
  parents[k] = k
end

def findparent(parents, node)
  if parents[node] != node then
    parent = findparent(parents, parents[node])
    parents[node] = parent
  end

  return parents[node]
end

# 2. iterate over edges. the smallest node is going to be the parent
graph.keys.each do |n1|
  root1 = findparent(parents, n1)
  graph[n1].each do |n2|
    root2 = findparent(parents, n2)

    if root1 < root2 then
      parents[root2] = root1
    else
      parents[root1] = root2
    end
  end
end

# 3. for each node, group it by its parent

tally = {}

graph.keys.each do |node|
  parent = findparent(parents, node)
  tally[parent] = (tally[parent] || 0) + 1
end

puts tally

# the answer should be the product of these two

puts "part 1: ", tally.values.inject(:*)
