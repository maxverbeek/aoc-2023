function string:split(delim)
	local lines = {}
	for line in self:gmatch("[^" .. delim .. "]+") do
		table.insert(lines, line)
	end

	return lines
end

function pprint(tbl, indent)
	indent = indent or 0
	for k, v in pairs(tbl) do
		formatting = string.rep("  ", indent) .. tostring(k) .. ": "
		if type(v) == "table" then
			print(formatting)
			pprint(v, indent + 1)
		else
			print(formatting .. (type(v) == "string" and string.format("%q", v) or tostring(v)))
		end
	end
end

function string:read_nums()
	local nums = {}
	for n in self:gmatch("(%d+)") do
		table.insert(nums, tonumber(n))
	end

	return nums
end

function isvalid(springs, patterns)
	local sprlength = 0
	local sprindex = 1

	for i = 1, #springs do
		local c = springs:sub(i, i)

		if c == "?" then
			-- probably messed up
			return false
		end

		if c == "." and sprlength ~= 0 then
			-- just terminated a spring
			if patterns[sprindex] ~= sprlength then
				return false
			end

			sprindex = sprindex + 1
			sprlength = 0
		elseif c == "#" then
			if sprindex > #patterns then
				-- encountered a spring, but there are no more springs
				return false
			end

			sprlength = sprlength + 1
		end
	end

	-- still have to check the last spring
	return (sprindex == #patterns and patterns[sprindex] == sprlength) or (sprindex == #patterns + 1 and sprlength == 0)
end

local count
function count(springs, patterns)
	local i = springs:find("?")

	if i ~= nil then
		-- try .
		local replaced_dot = ("%s%s%s"):format(springs:sub(1, i - 1), ".", springs:sub(i + 1))
		local dotcount = count(replaced_dot, patterns)
		-- try #
		local replaced_hash = ("%s%s%s"):format(springs:sub(1, i - 1), "#", springs:sub(i + 1))
		local hashcount = count(replaced_hash, patterns)

		return dotcount + hashcount
	end

	if isvalid(springs, patterns) then
		return 1
	else
		return 0
	end
end

function count_possibilities(line)
	local springs, patterns = line:match("(.+) (.+)")
	local nums = patterns:read_nums()

	return springs, nums
end

local sum = 0

local lines = io.read("*all"):split("\n")

for i, line in pairs(lines) do
	local springs, nums = count_possibilities(line)
	local p = count(springs, nums)
	print(p)
	sum = sum + p
end

function dupe5(springs, nums)
	local nums5 = {}
	for i = 1, 5 do
		for j = 1, #nums do
			nums5[i * j] = nums[j]
		end
	end

	return springs:rep(5), nums5
end

-- bruh

-- local sum = 0
--
-- local lines = io.read("*all"):split("\n")
--
-- for i, line in pairs(lines) do
-- 	local springs, nums = count_possibilities(line)
-- 	local springs5, nums5 = dupe5(springs, nums)
-- 	local p = count(springs5, nums5)
-- 	print(p)
-- 	sum = sum + p
-- end
