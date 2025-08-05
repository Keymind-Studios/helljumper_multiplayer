
---------------------------------------------------------
---------------- Auto Bundled Code Block ----------------
---------------------------------------------------------

do
    local searchers = package.searchers or package.loaders
    local origin_seacher = searchers[2]
    searchers[2] = function(path)
        local files =
        {
------------------------
-- Modules part begin --
------------------------

["inspect"] = function()
--------------------
-- Module: 'inspect'
--------------------
local inspect ={
  _VERSION = 'inspect.lua 3.1.0',
  _URL     = 'http://github.com/kikito/inspect.lua',
  _DESCRIPTION = 'human-readable representations of tables',
  _LICENSE = [[
    MIT LICENSE

    Copyright (c) 2013 Enrique García Cota

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

local tostring = tostring

inspect.KEY       = setmetatable({}, {__tostring = function() return 'inspect.KEY' end})
inspect.METATABLE = setmetatable({}, {__tostring = function() return 'inspect.METATABLE' end})

local function rawpairs(t)
  return next, t, nil
end

-- Apostrophizes the string if it has quotes, but not aphostrophes
-- Otherwise, it returns a regular quoted string
local function smartQuote(str)
  if str:match('"') and not str:match("'") then
    return "'" .. str .. "'"
  end
  return '"' .. str:gsub('"', '\\"') .. '"'
end

-- \a => '\\a', \0 => '\\0', 31 => '\31'
local shortControlCharEscapes = {
  ["\a"] = "\\a",  ["\b"] = "\\b", ["\f"] = "\\f", ["\n"] = "\\n",
  ["\r"] = "\\r",  ["\t"] = "\\t", ["\v"] = "\\v"
}
local longControlCharEscapes = {} -- \a => nil, \0 => \000, 31 => \031
for i=0, 31 do
  local ch = string.char(i)
  if not shortControlCharEscapes[ch] then
    shortControlCharEscapes[ch] = "\\"..i
    longControlCharEscapes[ch]  = string.format("\\%03d", i)
  end
end

local function escape(str)
  return (str:gsub("\\", "\\\\")
             :gsub("(%c)%f[0-9]", longControlCharEscapes)
             :gsub("%c", shortControlCharEscapes))
end

local function isIdentifier(str)
  return type(str) == 'string' and str:match( "^[_%a][_%a%d]*$" )
end

local function isSequenceKey(k, sequenceLength)
  return type(k) == 'number'
     and 1 <= k
     and k <= sequenceLength
     and math.floor(k) == k
end

local defaultTypeOrders = {
  ['number']   = 1, ['boolean']  = 2, ['string'] = 3, ['table'] = 4,
  ['function'] = 5, ['userdata'] = 6, ['thread'] = 7
}

local function sortKeys(a, b)
  local ta, tb = type(a), type(b)

  -- strings and numbers are sorted numerically/alphabetically
  if ta == tb and (ta == 'string' or ta == 'number') then return a < b end

  local dta, dtb = defaultTypeOrders[ta], defaultTypeOrders[tb]
  -- Two default types are compared according to the defaultTypeOrders table
  if dta and dtb then return defaultTypeOrders[ta] < defaultTypeOrders[tb]
  elseif dta     then return true  -- default types before custom ones
  elseif dtb     then return false -- custom types after default ones
  end

  -- custom types are sorted out alphabetically
  return ta < tb
end

-- For implementation reasons, the behavior of rawlen & # is "undefined" when
-- tables aren't pure sequences. So we implement our own # operator.
local function getSequenceLength(t)
  local len = 1
  local v = rawget(t,len)
  while v ~= nil do
    len = len + 1
    v = rawget(t,len)
  end
  return len - 1
end

local function getNonSequentialKeys(t)
  local keys, keysLength = {}, 0
  local sequenceLength = getSequenceLength(t)
  for k,_ in rawpairs(t) do
    if not isSequenceKey(k, sequenceLength) then
      keysLength = keysLength + 1
      keys[keysLength] = k
    end
  end
  table.sort(keys, sortKeys)
  return keys, keysLength, sequenceLength
end

local function countTableAppearances(t, tableAppearances)
  tableAppearances = tableAppearances or {}

  if type(t) == 'table' then
    if not tableAppearances[t] then
      tableAppearances[t] = 1
      for k,v in rawpairs(t) do
        countTableAppearances(k, tableAppearances)
        countTableAppearances(v, tableAppearances)
      end
      countTableAppearances(getmetatable(t), tableAppearances)
    else
      tableAppearances[t] = tableAppearances[t] + 1
    end
  end

  return tableAppearances
end

local copySequence = function(s)
  local copy, len = {}, #s
  for i=1, len do copy[i] = s[i] end
  return copy, len
end

local function makePath(path, ...)
  local keys = {...}
  local newPath, len = copySequence(path)
  for i=1, #keys do
    newPath[len + i] = keys[i]
  end
  return newPath
end

local function processRecursive(process, item, path, visited)
  if item == nil then return nil end
  if visited[item] then return visited[item] end

  local processed = process(item, path)
  if type(processed) == 'table' then
    local processedCopy = {}
    visited[item] = processedCopy
    local processedKey

    for k,v in rawpairs(processed) do
      processedKey = processRecursive(process, k, makePath(path, k, inspect.KEY), visited)
      if processedKey ~= nil then
        processedCopy[processedKey] = processRecursive(process, v, makePath(path, processedKey), visited)
      end
    end

    local mt  = processRecursive(process, getmetatable(processed), makePath(path, inspect.METATABLE), visited)
    if type(mt) ~= 'table' then mt = nil end -- ignore not nil/table __metatable field
    setmetatable(processedCopy, mt)
    processed = processedCopy
  end
  return processed
end



-------------------------------------------------------------------

local Inspector = {}
local Inspector_mt = {__index = Inspector}

function Inspector:puts(...)
  local args   = {...}
  local buffer = self.buffer
  local len    = #buffer
  for i=1, #args do
    len = len + 1
    buffer[len] = args[i]
  end
end

function Inspector:down(f)
  self.level = self.level + 1
  f()
  self.level = self.level - 1
end

function Inspector:tabify()
  self:puts(self.newline, string.rep(self.indent, self.level))
end

function Inspector:alreadyVisited(v)
  return self.ids[v] ~= nil
end

function Inspector:getId(v)
  local id = self.ids[v]
  if not id then
    local tv = type(v)
    id              = (self.maxIds[tv] or 0) + 1
    self.maxIds[tv] = id
    self.ids[v]     = id
  end
  return tostring(id)
end

function Inspector:putKey(k)
  if isIdentifier(k) then return self:puts(k) end
  self:puts("[")
  self:putValue(k)
  self:puts("]")
end

function Inspector:putTable(t)
  if t == inspect.KEY or t == inspect.METATABLE then
    self:puts(tostring(t))
  elseif self:alreadyVisited(t) then
    self:puts('<table ', self:getId(t), '>')
  elseif self.level >= self.depth then
    self:puts('{...}')
  else
    if self.tableAppearances[t] > 1 then self:puts('<', self:getId(t), '>') end

    local nonSequentialKeys, nonSequentialKeysLength, sequenceLength = getNonSequentialKeys(t)
    local mt                = getmetatable(t)

    self:puts('{')
    self:down(function()
      local count = 0
      for i=1, sequenceLength do
        if count > 0 then self:puts(',') end
        self:puts(' ')
        self:putValue(t[i])
        count = count + 1
      end

      for i=1, nonSequentialKeysLength do
        local k = nonSequentialKeys[i]
        if count > 0 then self:puts(',') end
        self:tabify()
        self:putKey(k)
        self:puts(' = ')
        self:putValue(t[k])
        count = count + 1
      end

      if type(mt) == 'table' then
        if count > 0 then self:puts(',') end
        self:tabify()
        self:puts('<metatable> = ')
        self:putValue(mt)
      end
    end)

    if nonSequentialKeysLength > 0 or type(mt) == 'table' then -- result is multi-lined. Justify closing }
      self:tabify()
    elseif sequenceLength > 0 then -- array tables have one extra space before closing }
      self:puts(' ')
    end

    self:puts('}')
  end
end

function Inspector:putValue(v)
  local tv = type(v)

  if tv == 'string' then
    self:puts(smartQuote(escape(v)))
  elseif tv == 'number' or tv == 'boolean' or tv == 'nil' or
         tv == 'cdata' or tv == 'ctype' then
    self:puts(tostring(v))
  elseif tv == 'table' then
    self:putTable(v)
  else
    self:puts('<', tv, ' ', self:getId(v), '>')
  end
end

-------------------------------------------------------------------

function inspect.inspect(root, options)
  options       = options or {}

  local depth   = options.depth   or math.huge
  local newline = options.newline or '\n'
  local indent  = options.indent  or '  '
  local process = options.process

  if process then
    root = processRecursive(process, root, {}, {})
  end

  local inspector = setmetatable({
    depth            = depth,
    level            = 0,
    buffer           = {},
    ids              = {},
    maxIds           = {},
    newline          = newline,
    indent           = indent,
    tableAppearances = countTableAppearances(root)
  }, Inspector_mt)

  inspector:putValue(root)

  return table.concat(inspector.buffer)
end

setmetatable(inspect, { __call = function(_, ...) return inspect.inspect(...) end })

return inspect


end,

["glue"] = function()
--------------------
-- Module: 'glue'
--------------------

-- Lua extended vocabulary of basic tools.
-- Written by Cosmin Apreutesei. Public domain.
-- Modifications by Sled

local glue = {}

local min, max, floor, ceil, log =
	math.min, math.max, math.floor, math.ceil, math.log
---@diagnostic disable-next-line: deprecated
local select, unpack, pairs, rawget = select, unpack, pairs, rawget

--math -----------------------------------------------------------------------

function glue.round(x, p)
	p = p or 1
	return floor(x / p + .5) * p
end

function glue.floor(x, p)
	p = p or 1
	return floor(x / p) * p
end

function glue.ceil(x, p)
	p = p or 1
	return ceil(x / p) * p
end

glue.snap = glue.round

function glue.clamp(x, x0, x1)
	return min(max(x, x0), x1)
end

function glue.lerp(x, x0, x1, y0, y1)
	return y0 + (x-x0) * ((y1-y0) / (x1 - x0))
end

function glue.nextpow2(x)
	return max(0, 2^(ceil(log(x) / log(2))))
end

--varargs --------------------------------------------------------------------

if table.pack then
	glue.pack = table.pack
else
	function glue.pack(...)
		return {n = select('#', ...), ...}
	end
end

--always use this because table.unpack's default j is #t not t.n.
function glue.unpack(t, i, j)
	return unpack(t, i or 1, j or t.n or #t)
end

--tables ---------------------------------------------------------------------

---Count the keys in a table with an optional upper limit
---@param t table
---@param maxn integer
---@return integer
function glue.count(t, maxn)
	local maxn = maxn or (1/0)
	local n = 0
	for _ in pairs(t) do
		n = n + 1
		if n >= maxn then break end
	end
	return n
end

---Reverse keys with values
---@param t table<any, any>
---@return table
function glue.index(t)
	local dt={}
	for k,v in pairs(t) do dt[v]=k end
	return dt
end


local function desc_cmp(a, b) return a > b end

---Put keys in a list, optionally sorted
---@param t table
---@param cmp boolean
---@return string[] | integer[] | any[]
function glue.keys(t, cmp)
	local dt={}
	for k in pairs(t) do
		dt[#dt+1]=k
	end
	if cmp == true or cmp == 'asc' then
		table.sort(dt)
	elseif cmp == 'desc' then
		table.sort(dt, desc_cmp)
	elseif cmp then
		table.sort(dt, cmp)
	end
	return dt
end

---Stateless pairs() that iterate elements in key order
---@param t table
---@param cmp boolean
---@return function
function glue.sortedpairs(t, cmp)
	local kt = glue.keys(t, cmp or true)
	local i = 0
	return function()
		i = i + 1
		return kt[i], t[kt[i]]
	end
end

---Update a table with the contents of other table(s)
---@param dt table
---@param ... any
---@return table
function glue.update(dt,...)
	for i=1,select('#',...) do
		local t=select(i,...)
		if t then
			for k,v in pairs(t) do dt[k]=v end
		end
	end
	return dt
end

---Add the contents of other table(s) without overwriting
---@param dt table
---@param ... any
---@return table
function glue.merge(dt,...)
	for i=1,select('#',...) do
		local t=select(i,...)
		if t then
			for k,v in pairs(t) do
				if rawget(dt, k) == nil then dt[k]=v end
			end
		end
	end
	return dt
end

---Copy the content of a table and create a new one without references
---@param orig table
---@return table
function glue.deepcopy(orig)
	local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[glue.deepcopy(orig_key)] = glue.deepcopy(orig_value)
        end
        setmetatable(copy, glue.deepcopy(getmetatable(orig)))
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

---Get the value of a table field, and if the field is not present in the\
---table, create it as an empty table, and return it
---@param t table
---@param k any
---@param v0 any
---@return any
function glue.attr(t, k, v0)
	local v = t[k]
	if v == nil then
		if v0 == nil then
			v0 = {}
		end
		v = v0
		t[k] = v
	end
	return v
end

--lists ----------------------------------------------------------------------

--extend a list with the elements of other lists.
function glue.extend(dt,...)
	for j=1,select('#',...) do
		local t=select(j,...)
		if t then
			local j = #dt
			for i=1,#t do dt[j+i]=t[i] end
		end
	end
	return dt
end

--append non-nil arguments to a list.
function glue.append(dt,...)
	local j = #dt
	for i=1,select('#',...) do
		dt[j+i] = select(i,...)
	end
	return dt
end

--insert n elements at i, shifting elemens on the right of i (i inclusive)
--to the right.
local function insert(t, i, n)
	if n == 1 then --shift 1
		table.insert(t, i, false)
		return
	end
	for p = #t,i,-1 do --shift n
		t[p+n] = t[p]
	end
end

--remove n elements at i, shifting elements on the right of i (i inclusive)
--to the left.
local function remove(t, i, n)
	n = min(n, #t-i+1)
	if n == 1 then --shift 1
		table.remove(t, i)
		return
	end
	for p=i+n,#t do --shift n
		t[p-n] = t[p]
	end
	for p=#t,#t-n+1,-1 do --clean tail
		t[p] = nil
	end
end

--shift all the elements on the right of i (i inclusive) to the left
--or further to the right.
function glue.shift(t, i, n)
	if n > 0 then
		insert(t, i, n)
	elseif n < 0 then
		remove(t, i, -n)
	end
	return t
end

--map f over t or extract a column from a list of records.
function glue.map(t, f, ...)
	local dt = {}
	if #t == 0 then --treat as hashmap
		if type(f) == 'function' then
			for k,v in pairs(t) do
				dt[k] = f(k, v, ...)
			end
		else
			for k,v in pairs(t) do
				local sel = v[f]
				if type(sel) == 'function' then --method to apply
					dt[k] = sel(v, ...)
				else --field to pluck
					dt[k] = sel
				end
			end
		end
	else --treat as array
		if type(f) == 'function' then
			for i,v in ipairs(t) do
				dt[i] = f(v, ...)
			end
		else
			for i,v in ipairs(t) do
				local sel = v[f]
				if type(sel) == 'function' then --method to apply
					dt[i] = sel(v, ...)
				else --field to pluck
					dt[i] = sel
				end
			end
		end
	end
	return dt
end

--arrays ---------------------------------------------------------------------

---Scan list for value, works with ffi arrays too given i and j
---@param v any
---@param t any[]
---@param eq fun(key:any, value:any)
---@param i integer
---@param j integer
---@return integer
function glue.indexof(v, t, eq, i, j)
	i = i or 1
	j = j or #t
	if eq then
		for i = i, j do
			if eq(t[i], v) then
				return i
			end
		end
	else
		for i = i, j do
			if t[i] == v then
				return i
			end
		end
	end
end

---Return the index of a table/array if value exists
---@param array any[]
---@param value any
function glue.arrayhas(array, value)
	for k,v in pairs(array) do
		if (v == value) then return k end
	end
	return nil
end

---Get new values of an array compared to another
---@param oldarray table
---@param newarray table
function glue.arraynv(oldarray, newarray)
	local newvalues = {}
	for k,v in pairs(newarray) do
		if (not glue.arrayhas(oldarray, v)) then
			glue.append(newvalues, v)
		end
	end
	return newvalues
end

---Reverse elements of a list in place, works with ffi arrays too given i and j
---@param t any[]
---@param i integer
---@param j integer
---@return any[]
function glue.reverse(t, i, j)
	i = i or 1
	j = (j or #t) + 1
	for k = 1, (j-i)/2 do
		t[i+k-1], t[j-k] = t[j-k], t[i+k-1]
	end
	return t
end

--- Get all the values of a key recursively
---@param t table
---@param dp any
function glue.childsbyparent(t, dp)
    for p,ch in pairs(t) do
		if (p == dp) then
			return ch
		end
		if (ch) then
			local found = glue.childsbyparent(ch, dp)
			if (found) then
				return found
			end
		end
    end
    return nil
end

-- Get the key of a value recursively
---@param t table
---@param dp any
function glue.parentbychild(t, dp)
    for p,ch in pairs(t) do
		if (ch[dp]) then
			return p
		end
		if (ch) then
			local found = glue.parentbychild(ch, dp)
			if (found) then
				return found
			end
		end
    end
    return nil
end

--- Split a list/array into small parts of given size
---@param list any[]
---@param chunks number
function glue.chunks(list, chunks)
	local chunkcounter = 0
	local chunk = {}
	local chunklist = {}
	-- Append chunks to the list in the specified amount of elements
	for k,v in pairs(list) do
		if (chunkcounter == chunks) then
			glue.append(chunklist, chunk)
			chunk = {}
			chunkcounter = 0
		end
		glue.append(chunk, v)
		chunkcounter = chunkcounter + 1
	end
	-- If there was a chunk that was not completed append it
	if (chunkcounter ~= 0) then
		glue.append(chunklist, chunk)
	end
	return chunklist
end

--binary search for an insert position that keeps the table sorted.
--works with ffi arrays too if lo and hi are provided.
local cmps = {}
cmps['<' ] = function(t, i, v) return t[i] <  v end
cmps['>' ] = function(t, i, v) return t[i] >  v end
cmps['<='] = function(t, i, v) return t[i] <= v end
cmps['>='] = function(t, i, v) return t[i] >= v end
local less = cmps['<']
function glue.binsearch(v, t, cmp, lo, hi)
	lo, hi = lo or 1, hi or #t
	cmp = cmp and cmps[cmp] or cmp or less
	local len = hi - lo + 1
	if len == 0 then return nil end
	if len == 1 then return not cmp(t, lo, v) and lo or nil end
	while lo < hi do
		local mid = floor(lo + (hi - lo) / 2)
		if cmp(t, mid, v) then
			lo = mid + 1
			if lo == hi and cmp(t, lo, v) then
				return nil
			end
		else
			hi = mid
		end
	end
	return lo
end

--strings --------------------------------------------------------------------

--string submodule. has its own namespace which can be merged with _G.string.
glue.string = {}

--- Split a string list/array given a separator string
function glue.string.split(s, sep)
    if (sep == nil or sep == '') then return 1 end
    local position, array = 0, {}
    for st, sp in function() return string.find(s, sep, position, true) end do
        table.insert(array, string.sub(s, position, st-1))
        position = sp + 1
    end
    table.insert(array, string.sub(s, position))
    return array
end

--split a string by a separator that can be a pattern or a plain string.
--return a stateless iterator for the pieces.
local function iterate_once(s, s1)
	return s1 == nil and s or nil
end
function glue.string.gsplit(s, sep, start, plain)
	start = start or 1
	plain = plain or false
	if not s:find(sep, start, plain) then
		return iterate_once, s:sub(start)
	end
	local done = false
	local function pass(i, j, ...)
		if i then
			local seg = s:sub(start, i - 1)
			start = j + 1
			return seg, ...
		else
			done = true
			return s:sub(start)
		end
	end
	return function()
		if done then return end
		if sep == '' then done = true; return s:sub(start) end
		return pass(s:find(sep, start, plain))
	end
end

--split a string into lines, optionally including the line terminator.
function glue.lines(s, opt)
	local term = opt == '*L'
	local patt = term and '([^\r\n]*()\r?\n?())' or '([^\r\n]*)()\r?\n?()'
	local next_match = s:gmatch(patt)
	local empty = s == ''
	local ended --string ended with no line ending
	return function()
		local s, i1, i2 = next_match()
		if s == nil then return end
		if s == '' and not empty and ended then s = nil end
		ended = i1 == i2
		return s
	end
end

--string trim12 from lua wiki.
function glue.string.trim(s)
	local from = s:match('^%s*()')
	return from > #s and '' or s:match('.*%S', from)
end

--escape a string so that it can be matched literally inside a pattern.
local function format_ci_pat(c)
	return ('[%s%s]'):format(c:lower(), c:upper())
end
function glue.string.esc(s, mode) --escape is a reserved word in Terra
	s = s:gsub('%%','%%%%'):gsub('%z','%%z')
		:gsub('([%^%$%(%)%.%[%]%*%+%-%?])', '%%%1')
	if mode == '*i' then s = s:gsub('[%a]', format_ci_pat) end
	return s
end

--string or number to hex.
function glue.string.tohex(s, upper)
	if type(s) == 'number' then
		return (upper and '%08.8X' or '%08.8x'):format(s)
	end
	if upper then
		return (s:gsub('.', function(c)
		  return ('%02X'):format(c:byte())
		end))
	else
		return (s:gsub('.', function(c)
		  return ('%02x'):format(c:byte())
		end))
	end
end

--hex to binary string.
function glue.string.fromhex(s)
	if #s % 2 == 1 then
		return glue.string.fromhex('0'..s)
	end
	return (s:gsub('..', function(cc)
	  return string.char(tonumber(cc, 16))
	end))
end

function glue.string.starts(s, p) --5x faster than s:find'^...' in LuaJIT 2.1
	return s:sub(1, #p) == p
end

function glue.string.ends(s, p)
	return p == '' or s:sub(-#p) == p
end

function glue.string.subst(s, t) --subst('{foo} {bar}', {foo=1, bar=2}) -> '1 2'
	return s:gsub('{([_%w]+)}', t)
end

--publish the string submodule in the glue namespace.
glue.update(glue, glue.string)

--iterators ------------------------------------------------------------------

--run an iterator and collect the n-th return value into a list.
local function select_at(i,...)
	return ...,select(i,...)
end
local function collect_at(i,f,s,v)
	local t = {}
	repeat
		v,t[#t+1] = select_at(i,f(s,v))
	until v == nil
	return t
end
local function collect_first(f,s,v)
	local t = {}
	repeat
		v = f(s,v); t[#t+1] = v
	until v == nil
	return t
end
function glue.collect(n,...)
	if type(n) == 'number' then
		return collect_at(n,...)
	else
		return collect_first(n,...)
	end
end

--closures -------------------------------------------------------------------

--no-op filters.
function glue.pass(...) return ... end
function glue.noop() end

--memoize for 0, 1, 2-arg and vararg and 1 retval functions.
local function memoize0(fn) --for strict no-arg functions
	local v, stored
	return function()
		if not stored then
			v = fn(); stored = true
		end
		return v
	end
end
local nilkey = {}
local nankey = {}
local function memoize1(fn) --for strict single-arg functions
	local cache = {}
	return function(arg)
		local k = arg == nil and nilkey or arg ~= arg and nankey or arg
		local v = cache[k]
		if v == nil then
			v = fn(arg); cache[k] = v == nil and nilkey or v
		else
			if v == nilkey then v = nil end
		end
		return v
	end
end
local function memoize2(fn) --for strict two-arg functions
	local cache = {}
	return function(a1, a2)
		local k1 = a1 ~= a1 and nankey or a1 == nil and nilkey or a1
		local cache2 = cache[k1]
		if cache2 == nil then
			cache2 = {}
			cache[k1] = cache2
		end
		local k2 = a2 ~= a2 and nankey or a2 == nil and nilkey or a2
		local v = cache2[k2]
		if v == nil then
			v = fn(a1, a2)
			cache2[k2] = v == nil and nilkey or v
		else
			if v == nilkey then v = nil end
		end
		return v
	end
end
local function memoize_vararg(fn, minarg, maxarg)
	local cache = {}
	local values = {}
	return function(...)
		local key = cache
		local narg = min(max(select('#',...), minarg), maxarg)
		for i = 1, narg do
			local a = select(i,...)
			local k = a ~= a and nankey or a == nil and nilkey or a
			local t = key[k]
			if not t then
				t = {}; key[k] = t
			end
			key = t
		end
		local v = values[key]
		if v == nil then
			v = fn(...); values[key] = v == nil and nilkey or v
		end
		if v == nilkey then v = nil end
		return v
	end
end
local memoize_narg = {[0] = memoize0, memoize1, memoize2}
local function choose_memoize_func(func, narg)
	if narg then
		local memoize_narg = memoize_narg[narg]
		if memoize_narg then
			return memoize_narg
		else
			return memoize_vararg, narg, narg
		end
	else
		local info = debug.getinfo(func, 'u')
		if info.isvararg then
			return memoize_vararg, info.nparams, 1/0
		else
			return choose_memoize_func(func, info.nparams)
		end
	end
end
function glue.memoize(func, narg)
	local memoize, minarg, maxarg = choose_memoize_func(func, narg)
	return memoize(func, minarg, maxarg)
end

--memoize a function with multiple return values.
function glue.memoize_multiret(func, narg)
	local memoize, minarg, maxarg = choose_memoize_func(func, narg)
	local function wrapper(...)
		return glue.pack(func(...))
	end
	local func = memoize(wrapper, minarg, maxarg)
	return function(...)
		return glue.unpack(func(...))
	end
end

local tuple_mt = {__call = glue.unpack}
function tuple_mt:__tostring()
	local t = {}
	for i=1,self.n do
		t[i] = tostring(self[i])
	end
	return string.format('(%s)', table.concat(t, ', '))
end
function glue.tuples(narg)
	return glue.memoize(function(...)
		return setmetatable(glue.pack(...), tuple_mt)
	end)
end

--objects --------------------------------------------------------------------

--set up dynamic inheritance by creating or updating a table's metatable.
function glue.inherit(t, parent)
	local meta = getmetatable(t)
	if meta then
		meta.__index = parent
	elseif parent ~= nil then
		setmetatable(t, {__index = parent})
	end
	return t
end

--prototype-based dynamic inheritance with __call constructor.
function glue.object(super, o, ...)
	o = o or {}
	o.__index = super
	o.__call = super and super.__call
	glue.update(o, ...) --add mixins, defaults, etc.
	return setmetatable(o, o)
end

local function install(self, combine, method_name, hook)
	rawset(self, method_name, combine(self[method_name], hook))
end
local function before(method, hook)
	if method then
		return function(self, ...)
			hook(self, ...)
			return method(self, ...)
		end
	else
		return hook
	end
end
function glue.before(self, method_name, hook)
	install(self, before, method_name, hook)
end
local function after(method, hook)
	if method then
		return function(self, ...)
			method(self, ...)
			return hook(self, ...)
		end
	else
		return hook
	end
end
function glue.after(self, method_name, hook)
	install(self, after, method_name, hook)
end
local function override(method, hook)
	local method = method or glue.noop
	return function(...)
		return hook(method, ...)
	end
end
function glue.override(self, method_name, hook)
	install(self, override, method_name, hook)
end

--return a metatable that supports virtual properties.
--can be used with setmetatable() and ffi.metatype().
function glue.gettersandsetters(getters, setters, super)
	local get = getters and function(t, k)
		local get = getters[k]
		if get then return get(t) end
		return super and super[k]
	end
	local set = setters and function(t, k, v)
		local set = setters[k]
		if set then set(t, v); return end
		rawset(t, k, v)
	end
	return {__index = get, __newindex = set}
end

--i/o ------------------------------------------------------------------------

--check if a file exists and can be opened for reading or writing.
function glue.canopen(name, mode)
	local f = io.open(name, mode or 'rb')
	if f then f:close() end
	return f ~= nil and name or nil
end

--read a file into a string (in binary mode by default).
function glue.readfile(name, mode, open)
	open = open or io.open
	local f, err = open(name, mode=='t' and 'r' or 'rb')
	if not f then return nil, err end
	local s, err = f:read'*a'
	if s == nil then return nil, err end
	f:close()
	return s
end

--read the output of a command into a string.
function glue.readpipe(cmd, mode, open)
	return glue.readfile(cmd, mode, open or io.popen)
end

--like os.rename() but behaves like POSIX on Windows too.
if jit then

	local ffi = require'ffi'

	if ffi.os == 'Windows' then

		ffi.cdef[[
			int MoveFileExA(
				const char *lpExistingFileName,
				const char *lpNewFileName,
				unsigned long dwFlags
			);
			int GetLastError(void);
		]]

		local MOVEFILE_REPLACE_EXISTING = 1
		local MOVEFILE_WRITE_THROUGH    = 8
		local ERROR_FILE_EXISTS         = 80
		local ERROR_ALREADY_EXISTS      = 183

		function glue.replacefile(oldfile, newfile)
			if ffi.C.MoveFileExA(oldfile, newfile, 0) ~= 0 then
				return true
			end
			local err = ffi.C.GetLastError()
			if err == ERROR_FILE_EXISTS or err == ERROR_ALREADY_EXISTS then
				if ffi.C.MoveFileExA(oldfile, newfile,
					bit.bor(MOVEFILE_WRITE_THROUGH, MOVEFILE_REPLACE_EXISTING)) ~= 0
				then
					return true
				end
				err = ffi.C.GetLastError()
			end
			return nil, 'WinAPI error '..err
		end

	else

		function glue.replacefile(oldfile, newfile)
			return os.rename(oldfile, newfile)
		end

	end

end

--write a string, number, table or the results of a read function to a file.
--uses binary mode by default.
function glue.writefile(filename, s, mode, tmpfile)
	if tmpfile then
		local ok, err = glue.writefile(tmpfile, s, mode)
		if not ok then
			return nil, err
		end
		local ok, err = glue.replacefile(tmpfile, filename)
		if not ok then
			os.remove(tmpfile)
			return nil, err
		else
			return true
		end
	end
	local f, err = io.open(filename, mode=='t' and 'w' or 'wb')
	if not f then
		return nil, err
	end
	local ok, err
	if type(s) == 'table' then
		for i = 1, #s do
			ok, err = f:write(s[i])
			if not ok then break end
		end
	elseif type(s) == 'function' then
		local read = s
		while true do
			ok, err = xpcall(read, debug.traceback)
			if not ok or err == nil then break end
			ok, err = f:write(err)
			if not ok then break end
		end
	else --string or number
		ok, err = f:write(s)
	end
	f:close()
	if not ok then
		os.remove(filename)
		return nil, err
	else
		return true
	end
end

--virtualize the print function.
function glue.printer(out, format)
	format = format or tostring
	return function(...)
		local n = select('#', ...)
		for i=1,n do
			out(format((select(i, ...))))
			if i < n then
				out'\t'
			end
		end
		out'\n'
	end
end

--dates & timestamps ---------------------------------------------------------

--compute timestamp diff. to UTC because os.time() has no option for UTC.
function glue.utc_diff(t)
   local d1 = os.date( '*t', 3600 * 24 * 10)
   local d2 = os.date('!*t', 3600 * 24 * 10)
	d1.isdst = false
	return os.difftime(os.time(d1), os.time(d2))
end

--overloading os.time to support UTC and get the date components as separate args.
function glue.time(utc, y, m, d, h, M, s, isdst)
	if type(utc) ~= 'boolean' then --shift arg#1
		utc, y, m, d, h, M, s, isdst = nil, utc, y, m, d, h, M, s
	end
	if type(y) == 'table' then
		local t = y
		if utc == nil then utc = t.utc end
		y, m, d, h, M, s, isdst = t.year, t.month, t.day, t.hour, t.min, t.sec, t.isdst
	end
	local utc_diff = utc and glue.utc_diff() or 0
	if not y then
		return os.time() + utc_diff
	else
		s = s or 0
		local t = os.time{year = y, month = m or 1, day = d or 1, hour = h or 0,
			min = M or 0, sec = s, isdst = isdst}
		return t and t + s - floor(s) + utc_diff
	end
end

--get the time at the start of the week of a given time, plus/minus a number of weeks.
function glue.sunday(utc, t, offset)
	if type(utc) ~= 'boolean' then --shift arg#1
		utc, t, offset = false, utc, t
	end
	local d = os.date(utc and '!*t' or '*t', t)
	return glue.time(false, d.year, d.month, d.day - (d.wday - 1) + (offset or 0) * 7)
end

--get the time at the start of the day of a given time, plus/minus a number of days.
function glue.day(utc, t, offset)
	if type(utc) ~= 'boolean' then --shift arg#1
		utc, t, offset = false, utc, t
	end
	local d = os.date(utc and '!*t' or '*t', t)
	return glue.time(false, d.year, d.month, d.day + (offset or 0))
end

--get the time at the start of the month of a given time, plus/minus a number of months.
function glue.month(utc, t, offset)
	if type(utc) ~= 'boolean' then --shift arg#1
		utc, t, offset = false, utc, t
	end
	local d = os.date(utc and '!*t' or '*t', t)
	return glue.time(false, d.year, d.month + (offset or 0))
end

--get the time at the start of the year of a given time, plus/minus a number of years.
function glue.year(utc, t, offset)
	if type(utc) ~= 'boolean' then --shift arg#1
		utc, t, offset = false, utc, t
	end
	local d = os.date(utc and '!*t' or '*t', t)
	return glue.time(false, d.year + (offset or 0))
end

--error handling -------------------------------------------------------------

--allocation-free assert() with string formatting.
--NOTE: unlike standard assert(), this only returns the first argument
--to avoid returning the error message and it's args along with it so don't
--use it with functions returning multiple values when you want those values.
function glue.assert(v, err, ...)
	if v then return v end
	err = err or 'assertion failed!'
	if select('#',...) > 0 then
		err = string.format(err, ...)
	end
	error(err, 2)
end

--pcall with traceback. LuaJIT and Lua 5.2 only.
local function pcall_error(e)
	return debug.traceback('\n'..tostring(e))
end
function glue.pcall(f, ...)
	return xpcall(f, pcall_error, ...)
end

local function unprotect(ok, result, ...)
	if not ok then return nil, result, ... end
	if result == nil then result = true end --to distinguish from error.
	return result, ...
end


---Wrap a function that raises errors on failure into a function that follows\
---the Lua convention of returning nil, err on failure
---@param func function
---@return function
function glue.protect(func)
	return function(...)
		return unprotect(pcall(func, ...))
	end
end

--pcall with finally and except "clauses":
--		local ret,err = fpcall(function(finally, except)
--			local foo = getfoo()
--			finally(function() foo:free() end)
--			except(function(err) io.stderr:write(err, '\n') end)
--		emd)
--NOTE: a bit bloated at 2 tables and 4 closures. Can we reduce the overhead?
local function fpcall(f,...)
	local fint, errt = {}, {}
	local function finally(f) fint[#fint+1] = f end
	local function onerror(f) errt[#errt+1] = f end
	local function err(e)
		for i=#errt,1,-1 do errt[i](e) end
		for i=#fint,1,-1 do fint[i]() end
		return tostring(e) .. '\n' .. debug.traceback()
	end
	local function pass(ok,...)
		if ok then
			for i=#fint,1,-1 do fint[i]() end
		end
		return ok,...
	end
	return pass(xpcall(f, err, finally, onerror, ...))
end

function glue.fpcall(...)
	return unprotect(fpcall(...))
end

--fcall is like fpcall() but without the protection (i.e. raises errors).
local function assert_fpcall(ok, ...)
	if not ok then error(..., 2) end
	return ...
end
function glue.fcall(...)
	return assert_fpcall(fpcall(...))
end

--modules --------------------------------------------------------------------

--create a module table that dynamically inherits another module.
--naming the module returns the same module table for the same name.
function glue.module(name, parent)
	if type(name) ~= 'string' then
		name, parent = parent, name
	end
	if type(parent) == 'string' then
		parent = require(parent)
	end
	parent = parent or _M
	local parent_P = parent and assert(parent._P, 'parent module has no _P') or _G
	local M = package.loaded[name]
	if M then
		return M, M._P
	end
	local P = {__index = parent_P}
	M = {__index = parent, _P = P}
	P._M = M
	M._M = M
	P._P = P
	setmetatable(P, P)
	setmetatable(M, M)
	if name then
		package.loaded[name] = M
		P[name] = M
	end
---@diagnostic disable-next-line: deprecated
	setfenv(2, P)
	return M, P
end

--setup a module to load sub-modules when accessing specific keys.
function glue.autoload(t, k, v)
	local mt = getmetatable(t) or {}
	if not mt.__autoload then
		local old_index = mt.__index
	 	local submodules = {}
		mt.__autoload = submodules
		mt.__index = function(t, k)
			--overriding __index...
			if type(old_index) == 'function' then
				local v = old_index(t, k)
				if v ~= nil then return v end
			elseif type(old_index) == 'table' then
				local v = old_index[k]
				if v ~= nil then return v end
			end
			if submodules[k] then
				local mod
				if type(submodules[k]) == 'string' then
					mod = require(submodules[k]) --module
				else
					mod = submodules[k](k) --custom loader
				end
				submodules[k] = nil --prevent loading twice
				if type(mod) == 'table' then --submodule returned its module table
					assert(mod[k] ~= nil) --submodule has our symbol
					t[k] = mod[k]
				end
				return rawget(t, k)
			end
		end
		setmetatable(t, mt)
	end
	if type(k) == 'table' then
		glue.update(mt.__autoload, k) --multiple key -> module associations.
	else
		mt.__autoload[k] = v --single key -> module association.
	end
	return t
end

--portable way to get script's directory, based on arg[0].
--NOTE: the path is not absolute, but relative to the current directory!
--NOTE: for bundled executables, this returns the executable's directory.
local dir = rawget(_G, 'arg') and arg[0]
	and arg[0]:gsub('[/\\]?[^/\\]+$', '') or '' --remove file name
glue.bin = dir == '' and '.' or dir

--portable way to add more paths to package.path, at any place in the list.
--negative indices count from the end of the list like string.sub().
--index 'after' means 0.
function glue.luapath(path, index, ext)
	ext = ext or 'lua'
	index = index or 1
	local psep = package.config:sub(1,1) --'/'
	local tsep = package.config:sub(3,3) --';'
	local wild = package.config:sub(5,5) --'?'
	local paths = glue.collect(glue.gsplit(package.path, tsep, nil, true))
	path = path:gsub('[/\\]', psep) --normalize slashes
	if index == 'after' then index = 0 end
	if index < 1 then index = #paths + 1 + index end
	table.insert(paths, index,  path .. psep .. wild .. psep .. 'init.' .. ext)
	table.insert(paths, index,  path .. psep .. wild .. '.' .. ext)
	package.path = table.concat(paths, tsep)
end

--portable way to add more paths to package.cpath, at any place in the list.
--negative indices count from the end of the list like string.sub().
--index 'after' means 0.
function glue.cpath(path, index)
	index = index or 1
	local psep = package.config:sub(1,1) --'/'
	local tsep = package.config:sub(3,3) --';'
	local wild = package.config:sub(5,5) --'?'
	local ext = package.cpath:match('%.([%a]+)%'..tsep..'?') --dll | so | dylib
	local paths = glue.collect(glue.gsplit(package.cpath, tsep, nil, true))
	path = path:gsub('[/\\]', psep) --normalize slashes
	if index == 'after' then index = 0 end
	if index < 1 then index = #paths + 1 + index end
	table.insert(paths, index,  path .. psep .. wild .. '.' .. ext)
	package.cpath = table.concat(paths, tsep)
end

--allocation -----------------------------------------------------------------

--freelist for Lua tables.
local function create_table()
	return {}
end
function glue.freelist(create, destroy)
	create = create or create_table
	destroy = destroy or glue.noop
	local t = {}
	local n = 0
	local function alloc()
		local e = t[n]
		if e then
			t[n] = false
			n = n - 1
		end
		return e or create()
	end
	local function free(e)
		destroy(e)
		n = n + 1
		t[n] = e
	end
	return alloc, free
end

--ffi ------------------------------------------------------------------------

if jit then

local ffi = require'ffi'

--static, auto-growing buffer allocation pattern (ctype must be vla).
function glue.buffer(ctype)
	local vla = ffi.typeof(ctype)
	local buf, len = nil, -1
	return function(minlen)
		if minlen == false then
			buf, len = nil, -1
		elseif minlen > len then
			len = glue.nextpow2(minlen)
			buf = vla(len)
		end
		return buf, len
	end
end

--like glue.buffer() but preserves data on reallocations
--also returns minlen instead of capacity.
function glue.dynarray(ctype)
	local buffer = glue.buffer(ctype)
	local elem_size = ffi.sizeof(ctype, 1)
	local buf0, minlen0
	return function(minlen)
		local buf, len = buffer(minlen)
		if buf ~= buf0 and buf ~= nil and buf0 ~= nil then
			ffi.copy(buf, buf0, minlen0 * elem_size)
		end
		buf0, minlen0 = buf, minlen
		return buf, minlen
	end
end

local intptr_ct = ffi.typeof'intptr_t'
local intptrptr_ct = ffi.typeof'const intptr_t*'
local intptr1_ct = ffi.typeof'intptr_t[1]'
local voidptr_ct = ffi.typeof'void*'

--x86: convert a pointer's address to a Lua number.
local function addr32(p)
	return tonumber(ffi.cast(intptr_ct, ffi.cast(voidptr_ct, p)))
end

--x86: convert a number to a pointer, optionally specifying a ctype.
local function ptr32(ctype, addr)
	if not addr then
		ctype, addr = voidptr_ct, ctype
	end
	return ffi.cast(ctype, addr)
end

--x64: convert a pointer's address to a Lua number or possibly string.
local function addr64(p)
	local np = ffi.cast(intptr_ct, ffi.cast(voidptr_ct, p))
   local n = tonumber(np)
	if ffi.cast(intptr_ct, n) ~= np then
		--address too big (ASLR? tagged pointers?): convert to string.
		return ffi.string(intptr1_ct(np), 8)
	end
	return n
end

--x64: convert a number or string to a pointer, optionally specifying a ctype.
local function ptr64(ctype, addr)
	if not addr then
		ctype, addr = voidptr_ct, ctype
	end
	if type(addr) == 'string' then
		return ffi.cast(ctype, ffi.cast(voidptr_ct,
			ffi.cast(intptrptr_ct, addr)[0]))
	else
		return ffi.cast(ctype, addr)
	end
end

glue.addr = ffi.abi'64bit' and addr64 or addr32
glue.ptr = ffi.abi'64bit' and ptr64 or ptr32

end --if jit

if bit then

	local band, bor, bnot = bit.band, bit.bor, bit.bnot

	--extract the bool value of a bitmask from a value.
	function glue.getbit(from, mask)
		return band(from, mask) == mask
	end

	--set a single bit of a value without affecting other bits.
	function glue.setbit(over, mask, yes)
		return bor(yes and mask or 0, band(over, bnot(mask)))
	end

	local function bor_bit(bits, k, mask, strict)
		local b = bits[k]
		if b then
			return bit.bor(mask, b)
		elseif strict then
			error(string.format('invalid bit %s', k))
		else
			return mask
		end
	end
	function glue.bor(flags, bits, strict)
		local mask = 0
		if type(flags) == 'number' then
			return flags --passthrough
		elseif type(flags) == 'string' then
			for k in flags:gmatch'[^%s]+' do
				mask = bor_bit(bits, k, mask, strict)
			end
		elseif type(flags) == 'table' then
			for k,v in pairs(flags) do
				k = type(k) == 'number' and v or k
				mask = bor_bit(bits, k, mask, strict)
			end
		else
			error'flags expected'
		end
		return mask
	end

end

return glue

end,

["blam"] = function()
--------------------
-- Module: 'blam'
--------------------
------------------------------------------------------------------------------
-- Blam! library for Chimera/SAPP Lua scripting
-- Sledmine, JerryBrick
-- Easier memory handle and provides standard functions for scripting
------------------------------------------------------------------------------
local cos = math.cos
local sin = math.sin
local atan = math.atan
local pi = math.pi
math.atan2 = math.atan2 or function(y, x)
    return atan(y / x) + (x < 0 and pi or 0)
end
local atan2 = math.atan2
local sqrt = math.sqrt
local fmod = math.fmod
local rad = math.rad
local deg = math.deg

local blam = {_VERSION = "1.12.3"}

------------------------------------------------------------------------------
-- Useful functions for internal usage
------------------------------------------------------------------------------

-- From legacy glue library!
--- String or number to hex
local function tohex(s, upper)
    if type(s) == "number" then
        return (upper and "%08.8X" or "%08.8x"):format(s)
    end
    if upper then
        return (s:sub(".", function(c)
            return ("%02X"):format(c:byte())
        end))
    else
        return (s:gsub(".", function(c)
            return ("%02x"):format(c:byte())
        end))
    end
end

--- Hex to binary string
local function fromhex(s)
    if #s % 2 == 1 then
        return fromhex("0" .. s)
    end
    return (s:gsub("..", function(cc)
        return string.char(tonumber(cc, 16))
    end))
end

local function split(s, sep)
    if (sep == nil or sep == "") then
        return 1
    end
    local position, array = 0, {}
    for st, sp in function()
        return string.find(s, sep, position, true)
    end do
        table.insert(array, string.sub(s, position, st - 1))
        position = sp + 1
    end
    table.insert(array, string.sub(s, position))
    return array
end

local null = 0xFFFFFFFF

--- Get if given value equals a null value in game engine terms
---@param value any
---@return boolean
function blam.isNull(value)
    if value == 0xFF or value == 0xFFFF or value == null or value == nil then
        return true
    end
    return false
end
local isNull = blam.isNull

---Return if game instance is host
---@return boolean
function blam.isGameHost()
    return server_type == "local"
end

---Return if game instance is single player
---@return boolean
function blam.isGameSinglePlayer()
    return server_type == "none"
end

---Return if the game instance is running on a dedicated server or connected as a "network client"
---@return boolean
function blam.isGameDedicated()
    return server_type == "dedicated"
end

---Return if the game instance is a SAPP server
---@return boolean
function blam.isGameSAPP()
    return register_callback or server_type == "sapp"
end

------------------------------------------------------------------------------
-- Blam! engine data
------------------------------------------------------------------------------

---@alias tagId number

-- Engine address list
local addressList = {
    tagDataHeader = 0x40440000,
    cameraType = 0x00647498, -- from giraffe
    gamePaused = 0x004ACA79,
    gameOnMenus = 0x00622058,
    joystickInput = 0x64D998, -- from aLTis
    firstPerson = 0x40000EB8, -- from aLTis
    objectTable = 0x400506B4,
    deviceGroupsTable = 0x00816110,
    widgetsInstance = 0x6B401C,
    -- syncedNetworkObjects = 0x004F7FA2
    syncedNetworkObjects = 0x006226F0, -- pointer, from Vulpes
    screenResolution = 0x637CF0,
    currentWidgetIdAddress = 0x6B401C,
    cinematicGlobals = 0x0068c83c
}

-- Server side addresses adjustment
if blam.isGameSAPP() then
    addressList.deviceGroupsTable = 0x006E1C50
    addressList.objectTable = 0x4005062C
    addressList.syncedNetworkObjects = 0x00598020 -- not pointer cause cheat engine sucks
    addressList.cinematicGlobals = 0x005f506c
end

-- Tag classes values
---@enum tagClasses
local tagClasses = {
    actorVariant = "actv",
    actor = "actr",
    antenna = "ant!",
    biped = "bipd",
    bitmap = "bitm",
    cameraTrack = "trak",
    colorTable = "colo",
    continuousDamageEffect = "cdmg",
    contrail = "cont",
    damageEffect = "jpt!",
    decal = "deca",
    detailObjectCollection = "dobc",
    deviceControl = "ctrl",
    deviceLightFixture = "lifi",
    deviceMachine = "mach",
    device = "devi",
    dialogue = "udlg",
    effect = "effe",
    equipment = "eqip",
    flag = "flag",
    fog = "fog ",
    font = "font",
    garbage = "garb",
    gbxmodel = "mod2",
    globals = "matg",
    glow = "glw!",
    grenadeHudInterface = "grhi",
    hudGlobals = "hudg",
    hudMessageText = "hmt ",
    hudNumber = "hud#",
    itemCollection = "itmc",
    item = "item",
    lensFlare = "lens",
    lightVolume = "mgs2",
    light = "ligh",
    lightning = "elec",
    materialEffects = "foot",
    meter = "metr",
    modelAnimations = "antr",
    modelCollisiionGeometry = "coll",
    model = "mode",
    multiplayerScenarioDescription = "mply",
    object = "obje",
    particleSystem = "pctl",
    particle = "part",
    physics = "phys",
    placeholder = "plac",
    pointPhysics = "pphy",
    preferencesNetworkGame = "ngpr",
    projectile = "proj",
    scenarioStructureBsp = "sbsp",
    scenario = "scnr",
    scenery = "scen",
    shaderEnvironment = "senv",
    shaderModel = "soso",
    shaderTransparentChicagoExtended = "scex",
    shaderTransparentChicago = "schi",
    shaderTransparentGeneric = "sotr",
    shaderTransparentGlass = "sgla",
    shaderTransparentMeter = "smet",
    shaderTransparentPlasma = "spla",
    shaderTransparentWater = "swat",
    shader = "shdr",
    sky = "sky ",
    soundEnvironment = "snde",
    soundLooping = "lsnd",
    soundScenery = "ssce",
    sound = "snd!",
    spheroid = "boom",
    stringList = "str#",
    tagCollection = "tagc",
    uiWidgetCollection = "Soul",
    uiWidgetDefinition = "DeLa",
    unicodeStringList = "ustr",
    unitHudInterface = "unhi",
    unit = "unit",
    vehicle = "vehi",
    virtualKeyboard = "vcky",
    weaponHudInterface = "wphi",
    weapon = "weap",
    weatherParticleSystem = "rain",
    wind = "wind"
}

-- Blam object classes values
---@enum objectClasses
local objectClasses = {
    biped = 0,
    vehicle = 1,
    weapon = 2,
    equipment = 3,
    garbage = 4,
    projectile = 5,
    scenery = 6,
    machine = 7,
    control = 8,
    lightFixture = 9,
    placeHolder = 10,
    soundScenery = 11
}

-- Camera types
---@enum cameraTypes
local cameraTypes = {
    scripted = 1, -- 22192
    firstPerson = 2, -- 30400
    devcam = 3, -- 30704
    thirdPerson = 4, -- 31952
    deadCamera = 5 -- 23776
}

-- Netgame flag classes
---@enum netgameFlagClasses
local netgameFlagClasses = {
    ctfFlag = 0,
    ctfVehicle = 1,
    ballSpawn = 2,
    raceTrack = 3,
    raceVehicle = 4,
    vegasBank = 5,
    teleportFrom = 6,
    teleportTo = 7,
    hillFlag = 8
}

-- Game type classes
---@enum gameTypeClasses
local gameTypeClasses = {
    none = 0,
    ctf = 1,
    slayer = 2,
    oddball = 3,
    koth = 4,
    race = 5,
    terminator = 6,
    stub = 7,
    ignored1 = 8,
    ignored2 = 9,
    ignored3 = 10,
    ignored4 = 11,
    allGames = 12,
    allExceptCtf = 13,
    allExceptRaceCtf = 14
}

-- Multiplayer team classes
---@enum multiplayerTeamClasses
local multiplayerTeamClasses = {red = 0, blue = 1}

-- Unit team classes
---@enum unitTeamClasses
local unitTeamClasses = {
    defaultByUnit = 0,
    player = 1,
    human = 2,
    covenant = 3,
    flood = 4,
    sentinel = 5,
    unused6 = 6,
    unused7 = 7,
    unused8 = 8,
    unused9 = 9
}

-- Object network role classes
---@enum objectNetworkRoleClasses
local objectNetworkRoleClasses = {
    master = 0,
    puppet = 1,
    locallyControlledPuppet = 2,
    localOnly = 3
}

-- Standard console colors
local consoleColors = {
    success = {1, 0.235, 0.82, 0},
    warning = {1, 0.94, 0.75, 0.098},
    error = {1, 1, 0.2, 0.2},
    unknown = {1, 0.66, 0.66, 0.66}
}

-- Offset input from the joystick game data
local joystickInputs = {
    -- No zero values also pressed time until maxmimum byte size
    button1 = 0, -- Triangle
    button2 = 1, -- Circle
    button3 = 2, -- Cross
    button4 = 3, -- Square
    leftBumper = 4,
    rightBumper = 5,
    leftTrigger = 6,
    rightTrigger = 7,
    backButton = 8,
    startButton = 9,
    leftStick = 10,
    rightStick = 11,
    -- Multiple values on the same offset, check dPadValues table
    dPad = 96,
    -- Non zero values
    dPadUp = 100,
    dPadDown = 104,
    dPadLeft = 106,
    dPadRight = 102,
    dPadUpRight = 101,
    dPadDownRight = 103,
    dPadUpLeft = 107,
    dPadDownLeft = 105
    -- TODO Add joys axis
    -- rightJoystick = 30,
}

-- Values for the possible dPad values from the joystick inputs
local dPadValues = {
    noButton = 1020,
    upRight = 766,
    downRight = 768,
    upLeft = 772,
    downLeft = 770,
    left = 771,
    right = 767,
    down = 769,
    up = 765
}

local engineConstants = {defaultNetworkObjectsCount = 509}

-- Global variables

---	This is the current gametype that is running. If no gametype is running, this will be set to nil
---, possible values are: ctf, slayer, oddball, king, race.
---@type string | nil
gametype = gametype
---This is the index of the local player. This is a value between 0 and 15, this value does not
---match with player index in the server and is not instantly assigned after joining.
---@type number | nil
local_player_index = local_player_index
---This is the name of the current loaded map.
---@type string
map = map
---Return if the map has protected tags data.
---@type boolean
map_is_protected = map_is_protected
---This is the name of the script. If the script is a global script, it will be defined as the
---filename of the script. Otherwise, it will be the name of the map.
---@type string
script_name = script_name
---This is the script type, possible values are global or map.
---@type string
script_type = script_type
---@type '"none"' | '"local"' | '"dedicated"' | '"sapp"'
server_type = server_type
---Return whether or not the script is sandboxed. See Sandoboxed Scripts for more information.
---@deprecated
---@type boolean
sandboxed = sandboxed ---@diagnostic disable-line: deprecated

local backupFunctions = {}

backupFunctions.console_is_open = _G.console_is_open
backupFunctions.console_out = _G.console_out
backupFunctions.execute_script = _G.execute_script
backupFunctions.get_global = _G.get_global
-- backupFunctions.set_global = _G.set_global
backupFunctions.get_tag = _G.get_tag
backupFunctions.set_callback = _G.set_callback
backupFunctions.set_timer = _G.set_timer
backupFunctions.stop_timer = _G.stop_timer

backupFunctions.spawn_object = _G.spawn_object
backupFunctions.delete_object = _G.delete_object
backupFunctions.get_object = _G.get_object
backupFunctions.get_dynamic_player = _G.get_dynamic_player

backupFunctions.hud_message = _G.hud_message

backupFunctions.create_directory = _G.create_directory
backupFunctions.remove_directory = _G.remove_directory
backupFunctions.directory_exists = _G.directory_exists
backupFunctions.list_directory = _G.list_directory
backupFunctions.write_file = _G.write_file
backupFunctions.read_file = _G.read_file
backupFunctions.delete_file = _G.delete_file
backupFunctions.file_exists = _G.file_exists

------------------------------------------------------------------------------
-- Chimera API auto completion
-- EmmyLua autocompletion for some functions!
-- Functions below do not have a real implementation and are not supossed to be imported
------------------------------------------------------------------------------

---Attempt to spawn an object given tag class, tag path and coordinates.
---Given a tag id is also accepted.
---@overload fun(tagId: number, x: number, y: number, z: number):number
---@param tagClass tagClasses Type of the tag to spawn
---@param tagPath string Path of object to spawn
---@param x number
---@param y number
---@param z number
---@return number? objectId
function spawn_object(tagClass, tagPath, x, y, z)
end

---Attempt to get the address of a player unit object given player index, returning nil on failure.<br>
---If no argument is given, the address to the local player’s unit object is returned, instead.
---@param playerIndex? number
---@return number? objectAddress
function get_dynamic_player(playerIndex)
end

spawn_object = backupFunctions.spawn_object
get_dynamic_player = backupFunctions.get_dynamic_player

------------------------------------------------------------------------------
-- SAPP API bindings
------------------------------------------------------------------------------

---Write content to a text file given file path
---@param path string Path to the file to write
---@param content string Content to write into the file
---@return boolean, string? result True if successful otherwise nil, error
function write_file(path, content)
    local file, error = io.open(path, "w")
    if (not file) then
        return false, error
    end
    local success, err = file:write(content)
    file:close()
    if (not success) then
        os.remove(path)
        return false, err
    else
        return true
    end
end

---Read the contents from a file given file path.
---@param path string Path to the file to read
---@return boolean, string? content string if successful otherwise nil, error
function read_file(path)
    local file, error = io.open(path, "r")
    if (not file) then
        return false, error
    end
    local content, error = file:read("*a")
    if (content == nil) then
        return false, error
    end
    file:close()
    return content
end

---Attempt create a directory with the given path.
---
---An error will occur if the directory can not be created.
---@param path string Path to the directory to create
---@return boolean
function create_directory(path)
    local success, error = os.execute("mkdir " .. path)
    if (not success) then
        return false
    end
    return true
end

---Attempt to remove a directory with the given path.
---
---An error will occur if the directory can not be removed.
---@param path string Path to the directory to remove
---@return boolean
function remove_directory(path)
    local success, error = os.execute("rmdir -r " .. path)
    if (not success) then
        return false
    end
    return true
end

---Verify if a directory exists given directory path
---@param path string
---@return boolean
function directory_exists(path)
    print("directory_exists", path)
    return os.execute("dir \"" .. path .. "\" > nul") == 0
end

---List the contents from a directory given directory path
---@param path string
---@return nil | integer | table
function list_directory(path)
    -- TODO This needs a way to separate folders from files
    if (path) then
        local command = "dir \"" .. path .. "\" /B"
        local pipe = io.popen(command, "r")
        if pipe then
            local output = pipe:read("*a")
            if (output) then
                local items = split(output, "\n")
                for index, item in pairs(items) do
                    if (item and item == "") then
                        items[index] = nil
                    end
                end
                return items
            end
        end
    end
    return nil
end

---Delete a file given file path
---@param path string
---@return boolean
function delete_file(path)
    return os.remove(path)
end

---Return if a file exists given file path.
---@param path string
---@return boolean
function file_exists(path)
    local file = io.open(path, "r")
    if (file) then
        file:close()
        return true
    end
    return false
end

---Return the memory address of a tag given tagId or tagClass and tagPath
---@param tagIdOrTagType string | number
---@param tagPath? string
---@return number?
function get_tag(tagIdOrTagType, tagPath)
    if (not tagPath) then
        return lookup_tag(tagIdOrTagType)
    else
        return lookup_tag(tagIdOrTagType, tagPath)
    end
end

---Execute a custom Halo script.
---
---A script can be either a standalone Halo command or a Lisp-formatted Halo scripting block.
---@param command string
function execute_script(command)
    return execute_command(command)
end

---Return the address of the object memory given object id
---@param objectId number
---@return number?
function get_object(objectId)
    if (objectId) then
        local object_memory = get_object_memory(objectId)
        if (object_memory ~= 0) then
            return object_memory
        end
    end
    return nil
end

---Despawn an object given objectId. An error will occur if the object does not exist.
---@param objectId number
function delete_object(objectId)
    destroy_object(objectId)
end

---Output text to the console, optional text colors in decimal format.<br>
---Avoid sending console messages if console_is_open() is true to avoid annoying the player.
---@param message string | number
---@param red? number
---@param green? number
---@param blue? number
function console_out(message, red, green, blue)
    -- TODO Add color printing to this function on SAPP
    cprint(message)
end

---Output text to console as debug message.
---
---This function will only output text if the debug mode is enabled.
---@param message string
function console_debug(message)
    if DebugMode then
        console_out(message)
    end
end

---Return true if the player has the console open, always returns true on SAPP.
---@return boolean
function console_is_open()
    return true
end

---Get the value of a Halo scripting global.\
---An error will be triggered if the global is not found
---@param name string Name of the global variable to get from hsc
---@return boolean | number
function get_global(name)
    error("SAPP can not retrieve global variables as Chimera does.. yet!")
end

---Print message to player HUD.\
---Messages will be printed to console if SAPP uses this function
---@param message string
function hud_message(message)
    cprint(message)
end

---Set the callback for an event game from the game events available on Chimera
---@param event '"command"' | '"frame"' | '"preframe"' | '"map load"' | '"precamera"' | '"rcon message"' | '"tick"' | '"pretick"' | '"unload"'
---@param callback string Global function name to call when the event is triggered
function set_callback(event, callback)
    if event == "tick" then
        register_callback(cb["EVENT_TICK"], callback)
    elseif event == "pretick" then
        error("SAPP does not support pretick event")
    elseif event == "frame" then
        error("SAPP does not support frame event")
    elseif event == "preframe" then
        error("SAPP does not support preframe event")
    elseif event == "map load" then
        register_callback(cb["EVENT_GAME_START"], callback)
    elseif event == "precamera" then
        error("SAPP does not support precamera event")
    elseif event == "rcon message" then
        _G[callback .. "_rcon_message"] = function(playerIndex,
                                                   command,
                                                   environment,
                                                   password)
            return _G[callback](playerIndex, command, password)
        end
        register_callback(cb["EVENT_COMMAND"], callback .. "_rcon_message")
    elseif event == "command" then
        _G[callback .. "_command"] = function(playerIndex, command, environment)
            return _G[callback](playerIndex, command, environment)
        end
        register_callback(cb["EVENT_COMMAND"], callback .. "_command")
    elseif event == "unload" then
        register_callback(cb["EVENT_GAME_END"], callback)
    else
        error("Unknown event: " .. event)
    end
end

---Register a timer to be called every intervalMilliseconds.<br>
---The callback function will be called with the arguments passed after the callbackName.<br>
---
---**WARNING:** SAPP will not return a timerId, it will return nil instead so timers can not be stopped.
---@param intervalMilliseconds number
---@param globalFunctionCallbackName string
---@vararg any
---@return number?
function set_timer(intervalMilliseconds, globalFunctionCallbackName, ...)
    return timer(intervalMilliseconds, globalFunctionCallbackName, ...)
end

function stop_timer(timerId)
    error("SAPP does not support stopping timers")
end

if register_callback then
    -- Provide global server type variable on SAPP
    server_type = "sapp"
    print("Compatibility with Chimera Lua API has been loaded!")
else
    console_is_open = backupFunctions.console_is_open
    console_out = backupFunctions.console_out
    execute_script = backupFunctions.execute_script
    get_global = backupFunctions.get_global
    -- set_global = -- backupFunctions.set_global
    get_tag = backupFunctions.get_tag
    set_callback = backupFunctions.set_callback
    set_timer = backupFunctions.set_timer
    stop_timer = backupFunctions.stop_timer
    spawn_object = backupFunctions.spawn_object
    delete_object = backupFunctions.delete_object
    get_object = backupFunctions.get_object
    get_dynamic_player = backupFunctions.get_dynamic_player
    hud_message = backupFunctions.hud_message
    create_directory = backupFunctions.create_directory
    remove_directory = backupFunctions.remove_directory
    directory_exists = backupFunctions.directory_exists
    list_directory = backupFunctions.list_directory
    write_file = backupFunctions.write_file
    read_file = backupFunctions.read_file
    delete_file = backupFunctions.delete_file
    file_exists = backupFunctions.file_exists
end

------------------------------------------------------------------------------
-- Generic functions
------------------------------------------------------------------------------

--- Verify if the given variable is a number
---@param var any
---@return boolean
local function isNumber(var)
    return (type(var) == "number")
end

--- Verify if the given variable is a string
---@param var any
---@return boolean
local function isString(var)
    return (type(var) == "string")
end

--- Verify if the given variable is a boolean
---@param var any
---@return boolean
local function isBoolean(var)
    return (type(var) == "boolean")
end

--- Verify if the given variable is a table
---@param var any
---@return boolean
local function isTable(var)
    return (type(var) == "table")
end

--- Remove spaces and tabs from the beginning and the end of a string
---@param str string
---@return string
local function trim(str)
    return str:match("^%s*(.*)"):match("(.-)%s*$")
end

--- Verify if the value is valid
---@param var any
---@return boolean
local function isValid(var)
    return (var and var ~= "" and var ~= 0)
end

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

--- Convert tag class int to string
---@param tagClassInt number
---@return string?
local function tagClassFromInt(tagClassInt)
    if (tagClassInt) then
        local tagClassHex = tohex(tagClassInt)
        local tagClass = ""
        if (tagClassHex) then
            local byte = ""
            for char in string.gmatch(tagClassHex, ".") do
                byte = byte .. char
                if (#byte % 2 == 0) then
                    tagClass = tagClass .. string.char(tonumber(byte, 16))
                    byte = ""
                end
            end
        end
        return tagClass
    end
    return nil
end

--- Return a list of object indexes that are currently spawned, indexed by their object id.
---@return number[]
function blam.getObjects()
    local objects = {}
    for objectIndex = 0, 2047 do
        local object, objectId = blam.getObject(objectIndex)
        if object and objectId then
            objects[objectId] = objectIndex
            -- objects[objectIndex] = objectId
        end
    end
    return objects
end

-- Local reference to the original console_out function
local original_console_out = console_out

--- Print a console message. It also supports multi-line messages!
---@param message string
local function consoleOutput(message, ...)
    -- Put the extra arguments into a table
    local args = {...}

    if (message == nil or #args > 5) then
        consoleOutput(debug.traceback("Wrong number of arguments on console output function", 2),
                      consoleColors.error)
    end

    -- Output color
    local colorARGB = {1, 1, 1, 1}

    -- Get the output color from arguments table
    if (isTable(args[1])) then
        colorARGB = args[1]
    elseif (#args == 3 or #args == 4) then
        colorARGB = args
    end

    -- Set alpha channel if not set
    if (#colorARGB == 3) then
        table.insert(colorARGB, 1, 1)
    end

    if message then
        if (isString(message)) then
            -- Explode the string!!
            for line in message:gmatch("([^\n]+)") do
                -- Trim the line
                local trimmedLine = trim(line)

                -- Print the line
                original_console_out(trimmedLine, table.unpack(colorARGB))
            end
        else
            original_console_out(message, table.unpack(colorARGB))
        end
    end
end

--- Convert booleans to bits and bits to booleans
---@param bitOrBool number
---@return boolean | number
local function b2b(bitOrBool)
    if (bitOrBool == 1) then
        return true
    elseif (bitOrBool == 0) then
        return false
    elseif (bitOrBool == true) then
        return 1
    elseif (bitOrBool == false) then
        return 0
    end
    error("B2B error, expected boolean or bit value, got " .. tostring(bitOrBool) .. " " ..
              type(bitOrBool))
end

------------------------------------------------------------------------------
-- Data manipulation and binding
------------------------------------------------------------------------------

local typesOperations

local function readBit(address, propertyData)
    return b2b(read_bit(address, propertyData.bitLevel))
end

local function writeBit(address, propertyData, propertyValue)
    return write_bit(address, propertyData.bitLevel, b2b(propertyValue))
end

local function readByte(address)
    return read_byte(address)
end

local function writeByte(address, propertyData, propertyValue)
    return write_byte(address, propertyValue)
end

local function readShort(address)
    return read_short(address)
end

local function writeShort(address, propertyData, propertyValue)
    return write_short(address, propertyValue)
end

local function readWord(address)
    return read_word(address)
end

local function writeWord(address, propertyData, propertyValue)
    return write_word(address, propertyValue)
end

local function readInt(address)
    return read_int(address)
end

local function writeInt(address, propertyData, propertyValue)
    return write_int(address, propertyValue)
end

local function readDword(address)
    return read_dword(address)
end

local function writeDword(address, propertyData, propertyValue)
    return write_dword(address, propertyValue)
end

local function readFloat(address)
    return read_float(address)
end

local function writeFloat(address, propertyData, propertyValue)
    return write_float(address, propertyValue)
end

local function readChar(address)
    return read_char(address)
end

local function writeChar(address, propertyData, propertyValue)
    return write_char(address, propertyValue)
end

local function readString(address)
    return read_string(address)
end

local function writeString(address, propertyData, propertyValue)
    return write_string(address, propertyValue)
end

--- Return the string of a unicode string given address
---@param address number
---@param rawRead? boolean
---@return string
function blam.readUnicodeString(address, rawRead)
    local stringAddress
    if rawRead then
        stringAddress = address
    else
        stringAddress = read_dword(address)
    end
    local length = stringAddress / 2
    local output = ""
    -- TODO Refactor this to support full unicode char size
    for i = 1, length do
        local char = read_string(stringAddress + (i - 1) * 0x2)
        if char == "" then
            break
        end
        output = output .. char
    end
    return output
end

--- Writes a unicode string in a given address
---@param address number
---@param newString string
---@param rawWrite? boolean
function blam.writeUnicodeString(address, newString, rawWrite)
    local stringAddress
    if rawWrite then
        stringAddress = address
    else
        stringAddress = read_dword(address)
    end
    -- Allow cancelling writing when the new string is a boolean false value
    if newString == false then
        return
    end
    -- TODO Refactor this to support writing ASCII and Unicode strings
    for i = 1, #newString do
        write_string(stringAddress + (i - 1) * 0x2, newString:sub(i, i))
        if i == #newString then
            write_byte(stringAddress + #newString * 0x2, 0x0)
        end
    end
    if #newString == 0 then
        write_string(stringAddress, "")
    end
end

local function readPointerUnicodeString(address, propertyData)
    return blam.readUnicodeString(address)
end

local function readUnicodeString(address, propertyData)
    return blam.readUnicodeString(address, true)
end

local function writePointerUnicodeString(address, propertyData, propertyValue)
    return blam.writeUnicodeString(address, propertyValue)
end

local function writeUnicodeString(address, propertyData, propertyValue)
    return blam.writeUnicodeString(address, propertyValue, true)
end

local function readList(address, propertyData)
    local operation = typesOperations[propertyData.elementsType]
    local elementCount = read_word(address - 0x4)
    local addressList = read_dword(address) + 0xC
    if (propertyData.noOffset) then
        addressList = read_dword(address)
    end
    local list = {}
    for currentElement = 1, elementCount do
        list[currentElement] = operation.read(addressList +
                                                  (propertyData.jump * (currentElement - 1)))
    end
    return list
end

local function writeList(address, propertyData, propertyValue)
    local operation = typesOperations[propertyData.elementsType]
    local elementCount = read_word(address - 0x4)
    local addressList
    if (propertyData.noOffset) then
        addressList = read_dword(address)
    else
        addressList = read_dword(address) + 0xC
    end
    for currentElement = 1, elementCount do
        local elementValue = propertyValue[currentElement]
        if (elementValue) then
            -- Check if there are problems at sending property data here due to missing property data
            operation.write(addressList + (propertyData.jump * (currentElement - 1)), propertyData,
                            elementValue)
        else
            if (currentElement > #propertyValue) then
                break
            end
        end
    end
end

local function readTable(address, propertyData)
    local table = {}
    local elementsCount = read_dword(address - 0x4)
    local firstElement = read_dword(address)
    for elementPosition = 1, elementsCount do
        local elementAddress = firstElement + ((elementPosition - 1) * propertyData.jump)
        table[elementPosition] = {}
        for subProperty, subPropertyData in pairs(propertyData.rows) do
            local operation = typesOperations[subPropertyData.type]
            table[elementPosition][subProperty] = operation.read(elementAddress +
                                                                     subPropertyData.offset,
                                                                 subPropertyData)
        end
    end
    return table
end

local function writeTable(address, propertyData, propertyValue)
    local elementCount = read_dword(address - 0x4)
    local firstElement = read_dword(address)
    for currentElement = 1, elementCount do
        local elementAddress = firstElement + (currentElement - 1) * propertyData.jump
        if (propertyValue[currentElement]) then
            for subProperty, subPropertyValue in pairs(propertyValue[currentElement]) do
                local subPropertyData = propertyData.rows[subProperty]
                if (subPropertyData) then
                    local operation = typesOperations[subPropertyData.type]
                    operation.write(elementAddress + subPropertyData.offset, subPropertyData,
                                    subPropertyValue)
                end
            end
        else
            if (currentElement > #propertyValue) then
                break
            end
        end
    end
end

local function readTagReference(address)
    -- local tagClass = read_dword(address)
    -- local tagPathPointer = read_dword(address + 0x4)
    -- local tagPath = read_string(tagPathPointer)
    -- local unknown = read_dword(address + 0x8)
    local tagId = read_dword(address + 0xC)
    return tagId
end

local function writeTagReference(address, propertyData, tagId)
    -- TODO Attempt to validate tag classes and overwrite tag path pointer
    write_dword(address + 0xC, tagId)
end

local function safeReadUnicodeString(address)
    local size = read_dword(address)
    if size == 0 then
        return ""
    end
    return blam.readUnicodeString(address + 0xC)
end

local function safeWriteUnicodeString(address, propertyData, text)
    local size = read_dword(address)
    local text = text
    if #text > size then
        text = text:sub(1, size)
    end
    return blam.writeUnicodeString(address + 0xC, text)
end

-- Data types operations references
typesOperations = {
    bit = {read = readBit, write = writeBit},
    byte = {read = readByte, write = writeByte},
    short = {read = readShort, write = writeShort},
    word = {read = readWord, write = writeWord},
    int = {read = readInt, write = writeInt},
    dword = {read = readDword, write = writeDword},
    float = {read = readFloat, write = writeFloat},
    char = {read = readChar, write = writeChar},
    string = {read = readString, write = writeString},
    -- TODO This is not ok, a pointer type with subtyping should be implemented
    pustring = {read = readPointerUnicodeString, write = writePointerUnicodeString},
    ustring = {read = readUnicodeString, write = writeUnicodeString},
    list = {read = readList, write = writeList},
    table = {read = readTable, write = writeTable},
    tagref = {read = readTagReference, write = writeTagReference},
    sustring = {read = safeReadUnicodeString, write = safeWriteUnicodeString}
}

-- Magic luablam metatable
local dataBindingMetaTable = {
    __newindex = function(object, property, propertyValue)
        -- Get all the data related to property field
        local propertyData = object.structure[property]
        if (propertyData) then
            local operation = typesOperations[propertyData.type]
            local propertyAddress = object.address + propertyData.offset
            operation.write(propertyAddress, propertyData, propertyValue)
        else
            local errorMessage = "Unable to write an invalid property ('" .. property .. "')"
            error(debug.traceback(errorMessage, 2))
        end
    end,
    __index = function(object, property)
        local objectStructure = object.structure
        local propertyData = objectStructure[property]
        if (propertyData) then
            local operation = typesOperations[propertyData.type]
            local propertyAddress = object.address + propertyData.offset
            return operation.read(propertyAddress, propertyData)
        else
            local errorMessage = "Unable to read an invalid property ('" .. property .. "')"
            error(debug.traceback(errorMessage, 2))
        end
    end
}

------------------------------------------------------------------------------
-- Object functions
------------------------------------------------------------------------------

--- Create a bind table for a given address and structure
---@param address number
---@param struct table
---@return table
local function createBindTable(address, struct)
    -- Create object
    local object = {}

    -- Set up legacy values
    object.address = address
    object.structure = struct

    -- Set mechanisim to bind properties to memory
    setmetatable(object, dataBindingMetaTable)

    return object
end

--- Return a dump of a given LuaBlam object
---@param object table
---@return table
local function dumpObject(object)
    local dump = {}
    for k, v in pairs(object.structure) do
        dump[k] = object[k]
    end
    return dump
end

--- Return a extended parent structure with another given structure
---@param parent table
---@param structure table
---@return table
local function extendStructure(parent, structure)
    local extendedStructure = {}
    for k, v in pairs(parent) do
        extendedStructure[k] = v
    end
    for k, v in pairs(structure) do
        extendedStructure[k] = v
    end
    return extendedStructure
end

------------------------------------------------------------------------------
-- Object structures
------------------------------------------------------------------------------

---@class dataTable
---@field name string
---@field maxElements number
---@field elementSize number
---@field capacity number
---@field size number
---@field nextElementId number
---@field firstElementAddress number

local dataTableStructure = {
    name = {type = "string", offset = 0},
    maxElements = {type = "word", offset = 0x20},
    elementSize = {type = "word", offset = 0x22},
    -- padding1 = {size = 0x0A, offset = 0x24},
    capacity = {type = "word", offset = 0x2E},
    size = {type = "word", offset = 0x30},
    nextElementId = {type = "word", offset = 0x32},
    firstElementAddress = {type = "dword", offset = 0x34}
}

local deviceGroupsTableStructure = {
    name = {type = "string", offset = 0},
    maxElements = {type = "word", offset = 0x20},
    elementSize = {type = "word", offset = 0x22},
    firstElementAddress = {type = "dword", offset = 0x34}
}

---@class blamObject
---@field address number
---@field tagId number Object tag ID
---@field networkRoleClass number Object network role class
---@field isGhost boolean Set object in some type of ghost mode
---@field isOnGround boolean Is the object touching ground
---@field isNotAffectedByGravity boolean Enable/disable object gravity
---@field isInWater boolean Is the object touching on water
---@field isStationary boolean Is the object stationary
---@field dynamicShading boolean Enable disable dynamic shading for lightmaps
---@field isNotCastingShadow boolean Enable/disable object shadow casting
---@field isFrozen boolean Freeze/unfreeze object existence
---@field isOutSideMap boolean Is object outside/inside bsp
---@field isCollideable boolean Enable/disable object collision, does not work with bipeds or vehicles
---@field isBeingPickedUp boolean Is the object being picked up
---@field hasNoCollision boolean Enable/disable object collision, causes animation problems
---@field model number Gbxmodel tag ID
---@field scale number Object scale factor
---@field health number Current health of the object
---@field maxHealth number Maximum health of the object
---@field shield number Current shield of the object
---@field maxShield number Maximum shield of the object
---@field colorAUpperRed number Red color channel for A modifier
---@field colorAUpperGreen number Green color channel for A modifier
---@field colorAUpperBlue number Blue color channel for A modifier
---@field colorBUpperRed number Red color channel for B modifier
---@field colorBUpperGreen number Green color channel for B modifier
---@field colorBUpperBlue number Blue color channel for B modifier
---@field colorCUpperRed number Red color channel for C modifier
---@field colorCUpperGreen number Green color channel for C modifier
---@field colorCUpperBlue number Blue color channel for C modifier
---@field colorDUpperRed number Red color channel for D modifier
---@field colorDUpperGreen number Green color channel for D modifier
---@field colorDUpperBlue number Blue color channel for D modifier
---@field colorALowerRed number Red color channel for A modifier
---@field colorALowerGreen number Green color channel for A modifier
---@field colorALowerBlue number Blue color channel for A modifier
---@field colorBLowerRed number Red color channel for B modifier
---@field colorBLowerGreen number Green color channel for B modifier
---@field colorBLowerBlue number Blue color channel for B modifier
---@field colorCLowerRed number Red color channel for C modifier
---@field colorCLowerGreen number Green color channel for C modifier
---@field colorCLowerBlue number Blue color channel for C modifier
---@field colorDLowerRed number Red color channel for D modifier
---@field colorDLowerGreen number Green color channel for D modifier
---@field colorDLowerBlue number Blue color channel for D modifier
---@field x number Current position of the object on X axis
---@field y number Current position of the object on Y axis
---@field z number Current position of the object on Z axis
---@field xVel number Current velocity of the object on X axis
---@field yVel number Current velocity of the object on Y axis
---@field zVel number Current velocity of the object on Z axis
---@field vX number Current x value in first rotation vector
---@field vY number Current y value in first rotation vector
---@field vZ number Current z value in first rotation vector
---@field v2X number Current x value in second rotation vector
---@field v2Y number Current y value in second rotation vector
---@field v2Z number Current z value in second rotation vector
---@field yawVel number Current velocity of the object in yaw
---@field pitchVel number Current velocity of the object in pitch
---@field rollVel number Current velocity of the object in roll
---@field locationId number Current id of the location in the map
---@field boundingRadius number Radius amount of the object in radians
---@field class objectClasses Object type
---@field team number Object multiplayer team
---@field nameIndex number Index of object name in the scenario tag
---@field playerId number Current player id if the object
---@field ownerId number Current owner id of the object any other object id
---@field isApparentlyDead boolean Is the object apparently dead
---@field isSilentlyKilled boolean Is the object really dead
---@field animationTagId number Current animation tag ID
---@field animation number Current animation index
---@field animationFrame number Current animation frame
---@field isNotDamageable boolean Make the object undamageable
---@field shaderPermutationIndex number Current shader permutation index
---@field regionPermutation1 number
---@field regionPermutation2 number
---@field regionPermutation3 number
---@field regionPermutation4 number
---@field regionPermutation5 number
---@field regionPermutation6 number
---@field regionPermutation7 number
---@field regionPermutation8 number
---@field parentObjectId number

-- blamObject structure
local objectStructure = {
    tagId = {type = "dword", offset = 0x0},
    networkRoleClass = {type = "dword", offset = 0x4},
    isNotMoving = {type = "bit", offset = 0x8, bitLevel = 0},
    existanceTime = {type = "dword", offset = 0xC},
    isGhost = {type = "bit", offset = 0x10, bitLevel = 0},
    isOnGround = {type = "bit", offset = 0x10, bitLevel = 1},
    ---@deprecated
    ignoreGravity = {type = "bit", offset = 0x10, bitLevel = 2},
    isNotAffectedByGravity = {type = "bit", offset = 0x10, bitLevel = 2},
    isInWater = {type = "bit", offset = 0x10, bitLevel = 3},
    isStationary = {type = "bit", offset = 0x10, bitLevel = 5},
    hasNoCollision = {type = "bit", offset = 0x10, bitLevel = 7},
    dynamicShading = {type = "bit", offset = 0x10, bitLevel = 14},
    isNotCastingShadow = {type = "bit", offset = 0x10, bitLevel = 18},
    isFrozen = {type = "bit", offset = 0x10, bitLevel = 20},
    -- FIXME Deprecated property, should be erased at a major release later
    frozen = {type = "bit", offset = 0x10, bitLevel = 20},
    isCollideable = {type = "bit", offset = 0x10, bitLevel = 24},
    isBeingPickedUp = {type = "bit", offset = 0x10, bitLevel = 26},
    isOutSideMap = {type = "bit", offset = 0x12, bitLevel = 5},
    model = {type = "dword", offset = 0x34},
    scale = {type = "float", offset = 0xB0},
    health = {type = "float", offset = 0xE0},
    maxHealth = {type = "float", offset = 0xD8},
    shield = {type = "float", offset = 0xE4},
    maxShield = {type = "float", offset = 0xDC},
    ---@deprecated
    redA = {type = "float", offset = 0x1B8},
    ---@deprecated
    greenA = {type = "float", offset = 0x1BC},
    ---@deprecated
    blueA = {type = "float", offset = 0x1C0},
    colorAUpperRed = {type = "float", offset = 0x188},
    colorAUpperGreen = {type = "float", offset = 0x18C},
    colorAUpperBlue = {type = "float", offset = 0x190},
    colorBUpperRed = {type = "float", offset = 0x194},
    colorBUpperGreen = {type = "float", offset = 0x198},
    colorBUpperBlue = {type = "float", offset = 0x19C},
    colorCUpperRed = {type = "float", offset = 0x1A0},
    colorCUpperGreen = {type = "float", offset = 0x1A4},
    colorCUpperBlue = {type = "float", offset = 0x1A8},
    colorDUpperRed = {type = "float", offset = 0x1AC},
    colorDUpperGreen = {type = "float", offset = 0x1B0},
    colorDUpperBlue = {type = "float", offset = 0x1B4},
    colorALowerRed = {type = "float", offset = 0x1B8},
    colorALowerGreen = {type = "float", offset = 0x1BC},
    colorALowerBlue = {type = "float", offset = 0x1C0},
    colorBLowerRed = {type = "float", offset = 0x1C4},
    colorBLowerGreen = {type = "float", offset = 0x1C8},
    colorBLowerBlue = {type = "float", offset = 0x1CC},
    colorCLowerRed = {type = "float", offset = 0x1D0},
    colorCLowerGreen = {type = "float", offset = 0x1D4},
    colorCLowerBlue = {type = "float", offset = 0x1D8},
    colorDLowerRed = {type = "float", offset = 0x1DC},
    colorDLowerGreen = {type = "float", offset = 0x1E0},
    colorDLowerBlue = {type = "float", offset = 0x1E4},
    x = {type = "float", offset = 0x5C},
    y = {type = "float", offset = 0x60},
    z = {type = "float", offset = 0x64},
    xVel = {type = "float", offset = 0x68},
    yVel = {type = "float", offset = 0x6C},
    zVel = {type = "float", offset = 0x70},
    vX = {type = "float", offset = 0x74},
    vY = {type = "float", offset = 0x78},
    vZ = {type = "float", offset = 0x7C},
    v2X = {type = "float", offset = 0x80},
    v2Y = {type = "float", offset = 0x84},
    v2Z = {type = "float", offset = 0x88},
    -- FIXME Some order from this values is probaby wrong, expected order is pitch, yaw, roll
    yawVel = {type = "float", offset = 0x8C},
    pitchVel = {type = "float", offset = 0x90},
    rollVel = {type = "float", offset = 0x94},
    locationId = {type = "dword", offset = 0x98},
    boundingRadius = {type = "float", offset = 0xAC},
    ---@deprecated
    type = {type = "word", offset = 0xB4},
    class = {type = "word", offset = 0xB4},
    team = {type = "word", offset = 0xB8},
    nameIndex = {type = "word", offset = 0xBA},
    playerId = {type = "dword", offset = 0xC0},
    ---@deprecated
    parentId = {type = "dword", offset = 0xC4},
    ownerId = {type = "dword", offset = 0xC4},
    ---@deprecated
    isHealthEmpty = {type = "bit", offset = 0x106, bitLevel = 2},
    isApparentlyDead = {type = "bit", offset = 0x106, bitLevel = 2},
    isSilentlyKilled = {type = "bit", offset = 0x106, bitLevel = 5},
    animationTagId = {type = "dword", offset = 0xCC},
    animation = {type = "word", offset = 0xD0},
    animationFrame = {type = "word", offset = 0xD2},
    isNotDamageable = {type = "bit", offset = 0x106, bitLevel = 11},
    shaderPermutationIndex = {type = "word", offset = 0x176},
    regionPermutation1 = {type = "byte", offset = 0x180},
    regionPermutation2 = {type = "byte", offset = 0x181},
    regionPermutation3 = {type = "byte", offset = 0x182},
    regionPermutation4 = {type = "byte", offset = 0x183},
    regionPermutation5 = {type = "byte", offset = 0x184},
    regionPermutation6 = {type = "byte", offset = 0x185},
    regionPermutation7 = {type = "byte", offset = 0x186},
    regionPermutation8 = {type = "byte", offset = 0x187},
    parentObjectId = {type = "dword", offset = 0x11C}
}

local unitStructure = extendStructure(objectStructure, {
    ---@deprecated
    invisible = {type = "bit", offset = 0x204, bitLevel = 4},
    isCamoActive = {type = "bit", offset = 0x204, bitLevel = 4},
    isControllable = {type = "bit", offset = 0x204, bitLevel = 5},
    isPlayerNotAllowedToEntry = {type = "bit", offset = 0x204, bitLevel = 16},
    parentSeatIndex = {type = "word", offset = 0x2F0},
    weaponAnimationTypeIndex = {type = "byte", offset = 0x2A1},
    weaponSlot = {type = "byte", offset = 0x2F2},
    firstWeaponObjectId = {type = "dword", offset = 0x2F8},
    secondWeaponObjectId = {type = "dword", offset = 0x2FC},
    thirdWeaponObjectId = {type = "dword", offset = 0x300},
    fourthWeaponObjectId = {type = "dword", offset = 0x304},
    camoScale = {type = "float", offset = 0x37C}
})

---@class unit : blamObject
---@field isCamoActive boolean Unit camo state
---@field isControllable boolean Unit controllable state
---@field isPlayerNotAllowedToEntry boolean Unit player not allowed to entry
---@field parentSeatIndex number Unit parent seat index
---@field weaponAnimationTypeIndex number Unit weapon animation type index
---@field weaponSlot number Current unit weapon slot
---@field firstWeaponObjectId number First weapon object id
---@field secondWeaponObjectId number Second weapon object id
---@field thirdWeaponObjectId number Third weapon object id
---@field fourthWeaponObjectId number Fourth weapon object id
---@field camoScale number Unit camo scale

-- Biped structure (extends object structure)
local bipedStructure = extendStructure(unitStructure, {
    noDropItems = {type = "bit", offset = 0x204, bitLevel = 20},
    flashlight = {type = "bit", offset = 0x204, bitLevel = 19},
    cameraX = {type = "float", offset = 0x230},
    cameraY = {type = "float", offset = 0x234},
    cameraZ = {type = "float", offset = 0x238},
    crouchHold = {type = "bit", offset = 0x208, bitLevel = 0},
    jumpHold = {type = "bit", offset = 0x208, bitLevel = 1},
    actionKeyHold = {type = "bit", offset = 0x208, bitLevel = 14},
    actionKey = {type = "bit", offset = 0x208, bitLevel = 6},
    meleeKey = {type = "bit", offset = 0x208, bitLevel = 7},
    reloadKey = {type = "bit", offset = 0x208, bitLevel = 10},
    weaponPTH = {type = "bit", offset = 0x208, bitLevel = 11},
    weaponSTH = {type = "bit", offset = 0x208, bitLevel = 12},
    flashlightKey = {type = "bit", offset = 0x208, bitLevel = 4},
    grenadeHold = {type = "bit", offset = 0x208, bitLevel = 13},
    crouch = {type = "byte", offset = 0x2A0},
    shooting = {type = "float", offset = 0x284},
    zoomLevel = {type = "byte", offset = 0x320},
    ---@deprecated
    invisibleScale = {type = "float", offset = 0x37C},
    primaryNades = {type = "byte", offset = 0x31E},
    secondaryNades = {type = "byte", offset = 0x31F},
    isNotAffectedByGravity = {type = "bit", offset = 0x4CC, bitLevel = 2},
    ignoreCollision = {type = "bit", offset = 0x4CC, bitLevel = 3},
    landing = {type = "byte", offset = 0x508},
    bumpedObjectId = {type = "dword", offset = 0x4FC},
    vehicleObjectId = {type = "dword", offset = 0x11C},
    vehicleSeatIndex = {type = "word", offset = 0x2F0},
    walkingState = {type = "char", offset = 0x503},
    motionState = {type = "byte", offset = 0x4D2},
    mostRecentDamagerPlayer = {type = "dword", offset = 0x43C}
})

---@class biped : unit
---@field invisible boolean Biped invisible state
---@field noDropItems boolean Biped ability to drop items at dead
---@field ignoreCollision boolean Biped ignores collisiion
---@field flashlight boolean Biped has flaslight enabled
---@field cameraX number Current position of the biped  X axis
---@field cameraY number Current position of the biped  Y axis
---@field cameraZ number Current position of the biped  Z axis
---@field crouchHold boolean Biped is holding crouch action
---@field jumpHold boolean Biped is holding jump action
---@field actionKeyHold boolean Biped is holding action key
---@field actionKey boolean Biped pressed action key
---@field meleeKey boolean Biped pressed melee key
---@field reloadKey boolean Biped pressed reload key
---@field weaponPTH boolean Biped is holding primary weapon trigger
---@field weaponSTH boolean Biped is holding secondary weapon trigger
---@field flashlightKey boolean Biped pressed flashlight key
---@field grenadeHold boolean Biped is holding grenade action
---@field crouch number Is biped crouch
---@field shooting number Is biped shooting, 0 when not, 1 when shooting
---@field weaponSlot number Current biped weapon slot
---@field zoomLevel number Current biped weapon zoom level, 0xFF when no zoom, up to 255 when zoomed
---@field invisibleScale number Opacity amount of biped invisiblity
---@field primaryNades number Primary grenades count
---@field secondaryNades number Secondary grenades count
---@field isNotAffectedByGravity boolean Enable/disable biped gravity
---@field landing number Biped landing state, 0 when landing, stays on 0 when landing hard, null otherwise
---@field bumpedObjectId number Object ID that the biped is bumping, vehicles, bipeds, etc, keeps the previous value if not bumping a new object
---@field vehicleSeatIndex number Current vehicle seat index of this biped
---@field vehicleObjectId number Current vehicle objectId of this object
---@field walkingState number Biped walking state, 0 = not walking, 1 = walking, 2 = stoping walking, 3 = stationary
---@field motionState number Biped motion state, 0 = standing , 1 = walking , 2 = jumping/falling
---@field mostRecentDamagerPlayer number Id of the player that caused the most recent damage to this biped

local vehicleStructure = extendStructure(unitStructure, {
    isTireBlur = {type = "bit", offset = 0x4CC, bitLevel = 0},
    isHovering = {type = "bit", offset = 0x4CC, bitLevel = 1},
    isCrouched = {type = "bit", offset = 0x4CC, bitLevel = 2},
    isJumping = {type = "bit", offset = 0x4CC, bitLevel = 3},
    speed = {type = "float", offset = 0x4D4},
    slide = {type = "float", offset = 0x4D8},
    turn = {type = "float", offset = 0x4DC},
    tirePosition = {type = "float", offset = 0x4E0},
    threadPositionLeft = {type = "float", offset = 0x4E4},
    threadPositionRight = {type = "float", offset = 0x4E8},
    hover = {type = "float", offset = 0x4EC},
    thrust = {type = "float", offset = 0x4F0},
    hoverX = {type = "float", offset = 0x4FC},
    hoverY = {type = "float", offset = 0x500},
    hoverZ = {type = "float", offset = 0x504},
    respawnTimer = {type = "dword", offset = 0x5AC},
    respawnTime = {type = "word", offset = 0x5B0},
    respawnX = {type = "float", offset = 0x5B4},
    respawnY = {type = "float", offset = 0x5B8},
    respawnZ = {type = "float", offset = 0x5BC}
})

---@class vehicle : unit
---@field isTireBlur boolean Vehicle tire blur state
---@field isHovering boolean Vehicle hovering state
---@field isCrouched boolean Vehicle crouch state
---@field isJumping boolean Vehicle jumping state
---@field speed number Vehicle speed
---@field slide number Vehicle slide
---@field turn number Vehicle turn
---@field tirePosition number Vehicle tire position
---@field threadPositionLeft number Vehicle thread position left
---@field threadPositionRight number Vehicle thread position right
---@field hover number Vehicle hover
---@field thrust number Vehicle thrust
---@field hoverX number Vehicle hover X axis
---@field hoverY number Vehicle hover Y axis
---@field hoverZ number Vehicle hover Z axis
---@field respawnTimer number Vehicle respawn timer
---@field respawnTime number Vehicle respawn time
---@field respawnX number Vehicle respawn X axis
---@field respawnY number Vehicle respawn Y axis
---@field respawnZ number Vehicle respawn Z axis

-- Tag data header structure
local tagDataHeaderStructure = {
    array = {type = "dword", offset = 0x0},
    scenario = {type = "dword", offset = 0x4},
    count = {type = "word", offset = 0xC}
}

---@class tag
---@field class number Type of the tag
---@field index number Tag Index
---@field id number Tag ID
---@field path string Path of the tag
---@field data number Address of the tag data
---@field indexed boolean Is tag indexed on an external map file

-- Tag structure
local tagHeaderStructure = {
    class = {type = "dword", offset = 0x0},
    index = {type = "word", offset = 0xC},
    id = {type = "dword", offset = 0xC},
    path = {type = "dword", offset = 0x10},
    data = {type = "dword", offset = 0x14},
    indexed = {type = "dword", offset = 0x18}
}

---@class tagCollection
---@field count number Number of tags in the collection
---@field tagList table List of tags

-- tagCollection structure
local tagCollectionStructure = {
    count = {type = "byte", offset = 0x0},
    tagList = {type = "list", offset = 0x4, elementsType = "dword", jump = 0x10}
}

---@class unicodeStringList
---@field count number Number of unicode strings
---@field strings string[] List of unicode strings

-- UnicodeStringList structure
local unicodeStringListStructure = {
    count = {type = "byte", offset = 0x0},
    ---@deprecated
    stringList = {type = "list", offset = 0x4, elementsType = "pustring", jump = 0x14},
    -- Previous string list property works because of magic (well because of shit code haha)
    strings = {type = "list", offset = 0x4, elementsType = "sustring", jump = 0x14, noOffset = true}
}

---@class bitmapSequence
---@field name string
---@field firtBitmapIndex number
---@field bitmapCount number

---@class bitmap
---@field type number
---@field format number
---@field usage number
---@field usageFlags number
---@field detailFadeFactor number
---@field sharpenAmount number
---@field bumpHeight number
---@field spriteBudgetSize number
---@field spriteBudgetCount number
---@field colorPlateWidth number
---@field colorPlateHeight number 
---@field compressedColorPlate string
---@field processedPixelData string
---@field blurFilterSize number
---@field alphaBias number
---@field mipmapCount number
---@field spriteUsage number
---@field spriteSpacing number
---@field sequencesCount number
---@field sequences bitmapSequence[]
---@field bitmapsCount number
---@field bitmaps table

-- Bitmap structure
local bitmapStructure = {
    type = {type = "word", offset = 0x0},
    format = {type = "word", offset = 0x2},
    usage = {type = "word", offset = 0x4},
    usageFlags = {type = "word", offset = 0x6},
    detailFadeFactor = {type = "dword", offset = 0x8},
    sharpenAmount = {type = "dword", offset = 0xC},
    bumpHeight = {type = "dword", offset = 0x10},
    spriteBudgetSize = {type = "word", offset = 0x14},
    spriteBudgetCount = {type = "word", offset = 0x16},
    colorPlateWidth = {type = "word", offset = 0x18},
    colorPlateHeight = {type = "word", offset = 0x1A},
    -- compressedColorPlate = {offset = 0x1C},
    -- processedPixelData = {offset = 0x30},
    blurFilterSize = {type = "float", offset = 0x44},
    alphaBias = {type = "float", offset = 0x48},
    mipmapCount = {type = "word", offset = 0x4C},
    spriteUsage = {type = "word", offset = 0x4E},
    spriteSpacing = {type = "word", offset = 0x50},
    -- padding1 = {size = 0x2, offset = 0x52},
    sequencesCount = {type = "byte", offset = 0x54},
    sequences = {
        type = "table",
        offset = 0x58,
        jump = 0x40,
        rows = {
            name = {type = "string", offset = 0x0},
            firstBitmapIndex = {type = "word", offset = 0x20},
            bitmapCount = {type = "word", offset = 0x22}
            -- padding = {size = 0x10, offset = 0x24},
            --[[
            sprites = {
                type = "table",
                offset = 0x34,
                jump = 0x20,
                rows = {
                    bitmapIndex = {type = "word", offset = 0x0},
                    --padding1 = {size = 0x2, offset = 0x2},
                    --padding2 = {size = 0x4, offset = 0x4},
                    left = {type = "float", offset = 0x8},
                    right = {type = "float", offset = 0xC},
                    top = {type = "float", offset = 0x10},
                    bottom = {type = "float", offset = 0x14},
                    registrationX = {type = "float", offset = 0x18},
                    registrationY = {type = "float", offset = 0x1C}
                }
            }
            ]]
        }
    },
    bitmapsCount = {type = "byte", offset = 0x60},
    bitmaps = {
        type = "table",
        offset = 0x64,
        jump = 0x30,
        rows = {
            class = {type = "dword", offset = 0x0},
            width = {type = "word", offset = 0x4},
            height = {type = "word", offset = 0x6},
            depth = {type = "word", offset = 0x8},
            type = {type = "word", offset = 0xA},
            format = {type = "word", offset = 0xC},
            flags = {type = "word", offset = 0xE},
            x = {type = "word", offset = 0x10},
            y = {type = "word", offset = 0x12},
            mipmapCount = {type = "word", offset = 0x14},
            -- padding1 = {size = 0x2, offset = 0x16},
            pixelOffset = {type = "dword", offset = 0x18}
            -- padding2 = {size = 0x4, offset = 0x1C},
            -- padding3 = {size = 0x4, offset = 0x20},
            -- padding4 = {size = 0x4, offset= 0x24},
            -- padding5 = {size = 0x8, offset= 0x28}
        }
    }
}

---@class uiWidgetDefinitionChild
---@field widgetTag number Child uiWidgetDefinition reference
---@field name number Child widget name
---@field customControllerIndex number Custom controller index for this child widget
---@field verticalOffset number Offset in Y axis of this child, relative to the parent
---@field horizontalOffset number Offset in X axis of this child, relative to the parent

---@class uiWidgetDefinitionEventHandler
---@field eventType number Type of the event
---@field gameFunction number Game function of this event
---@field widgetTag number uiWidgetDefinition tag id of the event
---@field script string Name of the script function assigned to this event

---@class uiWidgetDefinition
---@field type number Type of widget
---@field controllerIndex number Index of the player controller
---@field name string Name of the widget
---@field boundsY number Top bound of the widget
---@field boundsX number Left bound of the widget
---@field height number Bottom bound of the widget
---@field width number Right bound of the widget
---@field backgroundBitmap number Tag ID of the background bitmap
---@field eventHandlers uiWidgetDefinitionEventHandler[] tag ID list of the child widgets
---@field unicodeStringListTag number Tag ID of the unicodeStringList from this widget
---@field fontTag number Tag ID of the font from this widget
---@field justification number Text justification of the text from this widget
---@field stringListIndex number Text index from the unicodeStringList tag from this widget
---@field textHorizontalOffset number Text offset in X axis from this widget
---@field textVerticalOffset number Text offset in Y axis from this widget
---@field childWidgetsCount number Number of child widgets
---@field childWidgets uiWidgetDefinitionChild[] List of the child widgets

local uiWidgetDefinitionStructure = {
    type = {type = "word", offset = 0x0},
    controllerIndex = {type = "word", offset = 0x2},
    name = {type = "string", offset = 0x4},
    boundsY = {type = "short", offset = 0x24},
    boundsX = {type = "short", offset = 0x26},
    height = {type = "short", offset = 0x28},
    width = {type = "short", offset = 0x2A},
    backgroundBitmap = {type = "word", offset = 0x44},
    eventHandlers = {
        type = "table",
        offset = 0x54,
        jump = 0x48,
        rows = {
            -- TODO Add real flags support, or a subtyping of table instead
            -- flags = {type = "number", offset = 0x0},
            eventType = {type = "word", offset = 0x4},
            gameFunction = {type = "word", offset = 0x6},
            widgetTag = {type = "tagref", offset = 0x8},
            soundEffectTag = {type = "tagref", offset = 0x18},
            script = {type = "string", offset = 0x28}
        }
    },
    unicodeStringListTag = {type = "tagref", offset = 0xEC},
    fontTag = {type = "tagref", offset = 0xFC},
    -- TODO Add color support for hex and rgb values
    -- textColor = {type = "realargbcolor", offset = 0x10C},
    justification = {type = "word", offset = 0x11C},
    stringListIndex = {type = "short", offset = 0x12E},
    textHorizontalOffset = {type = "short", offset = 0x130},
    textVerticalOffset = {type = "short", offset = 0x132},
    ---@deprecated
    eventType = {type = "byte", offset = 0x03F0},
    ---@deprecated
    tagReference = {type = "word", offset = 0x400},
    childWidgetsCount = {type = "dword", offset = 0x03E0},
    ---@deprecated
    childWidgetsList = {type = "list", offset = 0x03E4, elementsType = "dword", jump = 0x50},
    childWidgets = {
        type = "table",
        offset = 0x03E4,
        jump = 0x50,
        rows = {
            widgetTag = {type = "tagref", offset = 0x0},
            name = {type = "string", offset = 0x10},
            -- flags = {type = "integer", offset = 0x30},
            customControllerIndex = {type = "short", offset = 0x34},
            verticalOffset = {type = "short", offset = 0x36},
            horizontalOffset = {type = "short", offset = 0x38}
        }
    }
}

---@class uiWidgetCollection
---@field count number Number of widgets in the collection
---@field tagList table Tag ID list of the widgets

-- uiWidgetCollection structure
local uiWidgetCollectionStructure = {
    count = {type = "byte", offset = 0x0},
    tagList = {type = "list", offset = 0x4, elementsType = "dword", jump = 0x10}
}

---@class crosshairOverlay
---@field x number
---@field y number
---@field widthScale number
---@field heightScale number
---@field defaultColorA number
---@field defaultColorR number
---@field defaultColorG number
---@field defaultColorB number
---@field sequenceIndex number

---@class crosshair
---@field type number
---@field mapType number
---@field bitmap number
---@field overlays crosshairOverlay[]

---@class weaponHudInterface
---@field childHud number
---@field totalAmmoCutOff number
---@field loadedAmmoCutOff number
---@field heatCutOff number
---@field ageCutOff number
---@field crosshairs crosshair[]

-- Weapon HUD Interface structure
local weaponHudInterfaceStructure = {
    childHud = {type = "dword", offset = 0xC},
    -- //TODO Check if this property should be moved to a nested property type
    usingParentHudFlashingParameters = {type = "bit", offset = "word", bitLevel = 1},
    -- padding1 = {type = "word", offset = 0x12},
    totalAmmoCutOff = {type = "word", offset = 0x14},
    loadedAmmoCutOff = {type = "word", offset = 0x16},
    heatCutOff = {type = "word", offset = 0x18},
    ageCutOff = {type = "word", offset = 0x1A},
    -- padding2 = {size = 0x20, offset = 0x1C},
    -- screenAlignment = {type = "word", },
    -- padding3 = {size = 0x2, offset = 0x3E},
    -- padding4 = {size = 0x20, offset = 0x40},
    crosshairs = {
        type = "table",
        offset = 0x88,
        jump = 0x68,
        rows = {
            type = {type = "word", offset = 0x0},
            mapType = {type = "word", offset = 0x2},
            -- padding1 = {size = 0x2, offset = 0x4},
            -- padding2 = {size = 0x1C, offset = 0x6},
            bitmap = {type = "dword", offset = 0x30},
            overlays = {
                type = "table",
                offset = 0x38,
                jump = 0x6C,
                rows = {
                    x = {type = "word", offset = 0x0},
                    y = {type = "word", offset = 0x2},
                    widthScale = {type = "float", offset = 0x4},
                    heightScale = {type = "float", offset = 0x8},
                    defaultColorB = {type = "byte", offset = 0x24},
                    defaultColorG = {type = "byte", offset = 0x25},
                    defaultColorR = {type = "byte", offset = 0x26},
                    defaultColorA = {type = "byte", offset = 0x27},
                    sequenceIndex = {type = "byte", offset = 0x46}
                }
            }
        }
    }
}

---@class spawnLocation
---@field x number
---@field y number
---@field z number
---@field rotation number
---@field type number
---@field teamIndex number

---@class vehicleLocation
---@field type number
---@field nameIndex string
---@field x number
---@field y number
---@field z number
---@field yaw number
---@field pitch number
---@field roll number

---@class cutsceneFlag
---@field name string
---@field x number
---@field y number
---@field z number
---@field vX number
---@field vY number

---@class scenarioScenery
---@field typeIndex number
---@field nameIndex string
---@field notPlaced boolean
---@field desiredPermutation number
---@field x number
---@field y number
---@field z number
---@field yaw number
---@field pitch number
---@field roll number

---@class scenarioBiped
---@field typeIndex number
---@field nameIndex string
---@field notPlaced boolean
---@field desiredPermutation number
---@field x number
---@field y number
---@field z number
---@field yaw number
---@field pitch number
---@field roll number

---@class scenario
---@field sceneryPaletteCount number Number of sceneries in the scenery palette
---@field sceneryPaletteList tagId[] Tag ID list of scenerys in the scenery palette
---@field spawnLocationCount number Number of spawns in the scenario
---@field spawnLocationList spawnLocation[] List of spawns in the scenario
---@field vehicleLocationCount number Number of vehicles locations in the scenario
---@field vehicleLocationList vehicleLocation[] List of vehicles locations in the scenario
---@field netgameEquipmentCount number Number of netgame equipments
---@field netgameEquipmentList table List of netgame equipments
---@field netgameFlagsCount number Number of netgame equipments
---@field netgameFlagsList table List of netgame equipments
---@field objectNamesCount number Count of the object names in the scenario
---@field objectNames string[] List of all the object names in the scenario
---@field sceneriesCount number Count of all the sceneries in the scenario
---@field sceneries scenarioScenery[] List of all the sceneries in the scenario
---@field bipedsCount number Count of all the bipeds in the scenario
---@field bipeds scenarioBiped[] List of all the bipeds in the scenario
---@field bipedPaletteCount number Count of all the bipeds in the biped palette
---@field bipedPaletteList tagId[] List of all the bipeds in the biped palette
---@field cutsceneFlagsCount number Count of all the cutscene flags in the scenario
---@field cutsceneFlags cutsceneFlag[] List of all the cutscene flags in the scenario
---@field actorPaletteCount number Count of all the actors in the actor palette
---@field encounterPaletteCount number Count of all the encounters in the encounter palette

-- Scenario structure
local scenarioStructure = {
    sceneryPaletteCount = {type = "byte", offset = 0x021C},
    sceneryPaletteList = {type = "list", offset = 0x0220, elementsType = "dword", jump = 0x30},
    spawnLocationCount = {type = "byte", offset = 0x354},
    spawnLocationList = {
        type = "table",
        offset = 0x358,
        jump = 0x34,
        rows = {
            x = {type = "float", offset = 0x0},
            y = {type = "float", offset = 0x4},
            z = {type = "float", offset = 0x8},
            rotation = {type = "float", offset = 0xC},
            teamIndex = {type = "byte", offset = 0x10},
            bspIndex = {type = "short", offset = 0x12},
            type = {type = "byte", offset = 0x14}
        }
    },
    vehicleLocationCount = {type = "byte", offset = 0x240},
    vehicleLocationList = {
        type = "table",
        offset = 0x244,
        jump = 0x78,
        rows = {
            type = {type = "word", offset = 0x0},
            nameIndex = {type = "word", offset = 0x2},
            x = {type = "float", offset = 0x8},
            y = {type = "float", offset = 0xC},
            z = {type = "float", offset = 0x10},
            yaw = {type = "float", offset = 0x14},
            pitch = {type = "float", offset = 0x18},
            roll = {type = "float", offset = 0x1C}
        }
    },
    netgameFlagsCount = {type = "byte", offset = 0x378},
    netgameFlagsList = {
        type = "table",
        offset = 0x37C,
        jump = 0x94,
        rows = {
            x = {type = "float", offset = 0x0},
            y = {type = "float", offset = 0x4},
            z = {type = "float", offset = 0x8},
            rotation = {type = "float", offset = 0xC},
            type = {type = "byte", offset = 0x10},
            teamIndex = {type = "word", offset = 0x12}
        }
    },
    netgameEquipmentCount = {type = "byte", offset = 0x384},
    netgameEquipmentList = {
        type = "table",
        offset = 0x388,
        jump = 0x90,
        rows = {
            levitate = {type = "bit", offset = 0x0, bitLevel = 0},
            type1 = {type = "word", offset = 0x4},
            type2 = {type = "word", offset = 0x6},
            type3 = {type = "word", offset = 0x8},
            type4 = {type = "word", offset = 0xA},
            teamIndex = {type = "byte", offset = 0xC},
            spawnTime = {type = "word", offset = 0xE},
            x = {type = "float", offset = 0x40},
            y = {type = "float", offset = 0x44},
            z = {type = "float", offset = 0x48},
            facing = {type = "float", offset = 0x4C},
            itemCollection = {type = "dword", offset = 0x5C}
        }
    },
    objectNamesCount = {type = "dword", offset = 0x204},
    objectNames = {
        type = "list",
        offset = 0x208,
        elementsType = "string",
        jump = 36,
        noOffset = true
    },
    sceneriesCount = {type = "dword", offset = 0x210},
    sceneries = {
        type = "table",
        offset = 0x214,
        jump = 0x48,
        rows = {
            typeIndex = {type = "word", offset = 0x0},
            nameIndex = {type = "word", offset = 0x2},
            notPlaced = {type = "bit", offset = 0x4, bitLevel = 0},
            desiredPermutation = {type = "byte", offset = 0x6},
            x = {type = "float", offset = 0x8},
            y = {type = "float", offset = 0xC},
            z = {type = "float", offset = 0x10},
            yaw = {type = "float", offset = 0x14},
            pitch = {type = "float", offset = 0x18},
            roll = {type = "float", offset = 0x1C}
        }
    },
    bipedsCount = {type = "dword", offset = 0x228},
    bipeds = {
        type = "table",
        offset = 0x0228 + 0x4,
        jump = 0x78,
        rows = {
            typeIndex = {type = "word", offset = 0x0},
            nameIndex = {type = "word", offset = 0x2},
            notPlaced = {type = "bit", offset = 0x4, bitLevel = 0},
            desiredPermutation = {type = "byte", offset = 0x6},
            x = {type = "float", offset = 0x8},
            y = {type = "float", offset = 0xC},
            z = {type = "float", offset = 0x10},
            yaw = {type = "float", offset = 0x14},
            pitch = {type = "float", offset = 0x18},
            roll = {type = "float", offset = 0x1C}
        }
    },
    bipedPaletteCount = {type = "byte", offset = 0x0234},
    bipedPaletteList = {type = "list", offset = 0x0238, elementsType = "dword", jump = 0x30},
    cutsceneFlagsCount = {type = "dword", offset = 0x4E4},
    cutsceneFlags = {
        type = "table",
        offset = 0x4E8,
        jump = 92,
        rows = {
            name = {type = "string", offset = 0x4},
            x = {type = "float", offset = 0x24},
            y = {type = "float", offset = 0x28},
            z = {type = "float", offset = 0x2C},
            vX = {type = "float", offset = 0x30},
            vY = {type = "float", offset = 0x34}
        }
    },
    actorPaletteCount = {type = "dword", offset = 0x0420},
    encounterPaletteCount = {type = "dword", offset = 0x042C}
}

---@class scenery
---@field model number
---@field modifierShader number

-- Scenery structure
local sceneryStructure = {
    model = {type = "word", offset = 0x28 + 0xC},
    modifierShader = {type = "word", offset = 0x90 + 0xC}
}

---@class collisionGeometry
---@field vertexCount number Number of vertex in the collision geometry
---@field vertexList table List of vertex in the collision geometry

-- Collision Model structure
local collisionGeometryStructure = {
    vertexCount = {type = "byte", offset = 0x408},
    vertexList = {
        type = "table",
        offset = 0x40C,
        jump = 0x10,
        rows = {
            x = {type = "float", offset = 0x0},
            y = {type = "float", offset = 0x4},
            z = {type = "float", offset = 0x8}
        }
    }
}

---@class animationClass
---@field name string Name of the animation
---@field type number Type of the animation
---@field frameCount number Frame count of the animation
---@field nextAnimation number Next animation id of the animation
---@field sound number Sound id of the animation

---@class modelAnimations
---@field fpAnimationCount number Number of first-person animations
---@field fpAnimationList number[] List of first-person animations
---@field animationCount number Number of animations of the model
---@field animationList animationClass[] List of animations of the model

-- Model Animation structure
local modelAnimationsStructure = {
    fpAnimationCount = {type = "byte", offset = 0x90},
    fpAnimationList = {
        type = "list",
        offset = 0x94,
        noOffset = true,
        elementsType = "byte",
        jump = 0x2
    },
    animationCount = {type = "byte", offset = 0x74},
    animationList = {
        type = "table",
        offset = 0x78,
        jump = 0xB4,
        rows = {
            name = {type = "string", offset = 0x0},
            type = {type = "word", offset = 0x20},
            frameCount = {type = "byte", offset = 0x22},
            nextAnimation = {type = "byte", offset = 0x38},
            sound = {type = "byte", offset = 0x3C}
        }
    }
}

---@class weapon : blamObject
---@field pressedReloadKey boolean Is weapon trying to reload
---@field isWeaponPunching boolean Is weapon playing melee or grenade animation
---@field ownerObjectId number Object ID of the weapon owner
---@field carrierObjectId number Object ID of the weapon owner
---@field isInInventory boolean Is weapon in inventory
---@field primaryTriggerState number Primary trigger state of the weapon
---@field totalAmmo number Total ammo of the weapon
---@field loadedAmmo number Loaded ammo of the weapon   

local weaponStructure = extendStructure(objectStructure, {
    pressedReloadKey = {type = "bit", offset = 0x230, bitLevel = 3},
    isWeaponPunching = {type = "bit", offset = 0x230, bitLevel = 4},
    ownerObjectId = {type = "dword", offset = 0x11C}, -- deprecated
    carrierObjectId = {type = "dword", offset = 0x11C},
    isInInventory = {type = "bit", offset = 0x1F4, bitLevel = 0},
    primaryTriggerState = {type = "byte", offset = 0x261},
    totalAmmo = {type = "word", offset = 0x2B6},
    loadedAmmo = {type = "word", offset = 0x2B8}
})

---@class weaponTag
---@field model number Tag ID of the weapon model

-- Weapon structure
local weaponTagStructure = {model = {type = "dword", offset = 0x34}}

-- @class modelMarkers
-- @field name string
-- @field nodeIndex number
-- TODO Add rotation fields, check Guerilla tag
-- @field x number
-- @field y number
-- @field z number

---@class modelPermutation
---@field name string

---@class modelRegion
---@field name string
---@field permutationCount number
---@field permutationsList modelPermutation[]

---@class modelNode
---@field x number
---@field y number
---@field z number

---@class gbxModel
---@field nodeCount number Number of nodes
---@field nodeList modelNode[] List of the model nodes
---@field regionCount number Number of regions
---@field regionList modelRegion[] List of regions

-- Model structure
local modelStructure = {
    nodeCount = {type = "dword", offset = 0xB8},
    nodeList = {
        type = "table",
        offset = 0xBC,
        jump = 0x9C,
        rows = {
            x = {type = "float", offset = 0x28},
            y = {type = "float", offset = 0x2C},
            z = {type = "float", offset = 0x30}
        }
    },
    regionCount = {type = "dword", offset = 0xC4},
    regionList = {
        type = "table",
        offset = 0xC8,
        -- jump = 0x50,
        jump = 0x4C,
        rows = {
            name = {type = "string", offset = 0x0},
            permutationCount = {type = "dword", offset = 0x40},
            permutationsList = {
                type = "table",
                offset = 0x44,
                jump = 0x58,
                rows = {
                    name = {type = "string", offset = 0x0}
                    -- markersList = {
                    --    type = "table",
                    --    offset = 0x4C,
                    --    jump = 0x0,
                    --    rows = {
                    --        name = {type = "string", offset = 0x0},
                    --        nodeIndex = {type = "word", offset = 0x20}
                    --    }
                    -- }
                }
            }
        }
    }
}

---@class projectile : blamObject
---@field action number Enumeration of denotation action
---@field attachedToObjectId number Id of the attached object
---@field armingTimer number PENDING
---@field xVel number Velocity in x direction
---@field yVel number Velocity in y direction
---@field zVel number Velocity in z direction
---@field yaw number Rotation in yaw direction
---@field pitch number Rotation in pitch direction
---@field roll number Rotation in roll direction

-- Projectile structure
local projectileStructure = extendStructure(objectStructure, {
    action = {type = "word", offset = 0x230},
    attachedToObjectId = {type = "dword", offset = 0x11C},
    armingTimer = {type = "float", offset = 0x248},
    --[[xVel = {type = "float", offset = 0x254},
    yVel = {type = "float", offset = 0x258},
    zVel = {type = "float", offset = 0x25C},]]
    pitch = {type = "float", offset = 0x264},
    yaw = {type = "float", offset = 0x268},
    roll = {type = "float", offset = 0x26C}
})

---@class player
---@field id number Get playerId of this player
---@field host number Check if player is host, 0 when host, null when not
---@field name string Name of this player
---@field team number Team color of this player, 0 when red, 1 when on blue team
---@field interactionObjectId number Object ID of the object this player is interacting with
---@field interactonObjectType number Type of the object this player is interacting with
---@field interactionObjectSeat number Seat of the object this player is interacting with
---@field respawnTime number Time in ticks until this player respawns
---@field respawnGrowthTime number Time in ticks until this player respawns
---@field objectId number Return the objectId associated to this player
---@field lastObjectId number Return the last objectId associated to this player
---@field lastFireTime number Return the last fire time associated to this player
---@field name2 string Name of this player
---@field color number Color of the player, only works on "Free for All" gametypes
---@field machineIndex number Machine index of this player
---@field controllerIndex number Controller index of this player
---@field team2 number Team color of this player, 0 when red, 1 when on blue team
---@field index number Local index of this player 0-15
---@field invisibilityTime number Time in ticks until this player is invisible
---@field speed number Current speed of this player
---@field teleporterFlagId number Unknown
---@field objectiveMode number Unknown
---@field objectivePlayerId number Unknown
---@field targetPlayerId number Player id the player is looking at
---@field targetTime number Some timer for fading in the name of the player being looked at
---@field lastDeathTime number Time in ticks since this player last died
---@field slayerTargetPlayerId number Unknown
---@field oddManOut number Is player odd man out
---@field killStreak number Current kill streak of this player
---@field multiKill number Current multi kill of this player
---@field lastKillTime number Time in ticks since this player last killed
---@field kills number Kills quantity done by this player
---@field ping number Ping amount from server of this player in milliseconds
---@field assists number Assists count of this player
---@field betraysAndSuicides number Betrays plus suicides count of this player
---@field deaths number Deaths count of this player
---@field suicides number Suicides count of this player

local playerStructure = {
    id = {type = "word", offset = 0x0},
    host = {type = "word", offset = 0x2},
    name = {type = "ustring", forced = true, offset = 0x4},
    unknown = {type = "byte", offset = 0x1C},
    team = {type = "byte", offset = 0x20},
    unknown2 = {type = "byte", offset = 0x21},
    unknown3 = {type = "byte", offset = 0x22},
    unknown4 = {type = "byte", offset = 0x23},
    interactionObjectId = {type = "dword", offset = 0x24},
    interactionObjectType = {type = "word", offset = 0x28},
    interactionObjectSeat = {type = "word", offset = 0x2A},
    respawnTime = {type = "dword", offset = 0x2C},
    respawnGrowthTime = {type = "dword", offset = 0x30},
    objectId = {type = "dword", offset = 0x34},
    lastObjectId = {type = "dword", offset = 0x38},
    unknown5 = {type = "dword", offset = 0x3C},
    unknown6 = {type = "dword", offset = 0x40},
    lastFireTime = {type = "dword", offset = 0x44},
    name2 = {type = "ustring", forced = true, offset = 0x48},
    color = {type = "word", offset = 0x60},
    unknown7 = {type = "word", offset = 0x62},
    machineIndex = {type = "byte", offset = 0x64},
    controllerIndex = {type = "byte", offset = 0x65},
    team2 = {type = "byte", offset = 0x66},
    index = {type = "byte", offset = 0x67},
    invisibilityTime = {type = "word", offset = 0x68},
    unknown8 = {type = "word", offset = 0x6A},
    speed = {type = "float", offset = 0x6C},
    teleporterFlagId = {type = "dword", offset = 0x70}, -- Unknown
    objectiveMode = {type = "dword", offset = 0x74}, -- Unknown
    objectivePlayerId = {type = "dword", offset = 0x78}, -- Unknown
    targetPlayerId = {type = "dword", offset = 0x7C}, -- Player id the player is looking at?
    targetTime = {type = "dword", offset = 0x80}, -- Some timer for fading in the name of the player being looked at?
    lastDeathTime = {type = "dword", offset = 0x84},
    slayerTargetPlayerId = {type = "dword", offset = 0x88},
    oddManOut = {type = "dword", offset = 0x8C}, -- Player is odd man out
    unknown9 = {type = "dword", offset = 0x90},
    unknown10 = {type = "word", offset = 0x94},
    killStreak = {type = "word", offset = 0x96},
    multiKill = {type = "word", offset = 0x98},
    lastKillTime = {type = "word", offset = 0x9A},
    kills = {type = "word", offset = 0x9C},
    ping = {type = "dword", offset = 0xDC},
    assists = {type = "word", offset = 0XA4},
    betraysAndSuicides = {type = "word", offset = 0xAC},
    deaths = {type = "word", offset = 0xAE},
    suicides = {type = "word", offset = 0XB0},
    --[[
        Appears to be some kind of tick or packet counter, when defined to specific value it will
        cause the player to desync and show the "connection problems icon"
        Counts up to 31 and then resets to 0
    ]]
    unknownTimer1 = {type = "dword", offset = 0xE8}
}

---@class firstPersonInterface
---@field firstPersonHands number

---@class multiplayerInformation
---@field flag number Tag ID of the flag object used for multiplayer games
---@field unit number Tag ID of the unit object used for multiplayer games

---@class globalsTag
---@field multiplayerInformation multiplayerInformation[]
---@field firstPersonInterface firstPersonInterface[]

local globalsTagStructure = {
    multiplayerInformation = {
        type = "table",
        jump = 0x0,
        offset = 0x168,
        rows = {flag = {type = "dword", offset = 0xC}, unit = {type = "dword", offset = 0x1C}}
    },
    firstPersonInterface = {
        type = "table",
        jump = 0x0,
        offset = 0x180,
        rows = {firstPersonHands = {type = "dword", offset = 0xC}}
    }
}

---@class firstPerson
---@field weaponObjectId number Weapon Id from the first person view

local firstPersonStructure = {weaponObjectId = {type = "dword", offset = 0x10}}

---@class bipedTag
---@field model number Gbxmodel tag Id of this biped tag
---@field disableCollision boolean Disable collision of this biped tag
---@field weaponCount number Number of weapons of this biped
---@field weaponList tagId[] List of weapons of this biped

local bipedTagStructure = {
    model = {type = "dword", offset = 0x34},
    disableCollision = {type = "bit", offset = 0x2F4, bitLevel = 5},
    weaponCount = {type = "byte", offset = 0x02D8},
    weaponList = {type = "list", offset = 0x02D8 + 0x4, jump = 0x24, elementsType = "dword"}

}

---@class deviceMachine : blamObject
---@field powerGroupIndex number Power index from the device groups table
---@field power number Position amount of this device machine
---@field powerChange number Power change of this device machine
---@field positionGroupIndex number Power index from the device groups table
---@field position number Position amount of this device machine
---@field positionChange number Position change of this device machine
local deviceMachineStructure = extendStructure(objectStructure, {
    powerGroupIndex = {type = "word", offset = 0x1F8},
    power = {type = "float", offset = 0x1FC},
    powerChange = {type = "float", offset = 0x200},
    positionGroupIndex = {type = "word", offset = 0x204},
    position = {type = "float", offset = 0x208},
    positionChange = {type = "float", offset = 0x20C}
})

---@class hudGlobals
---@field anchor number
---@field x number
---@field y number
---@field width number
---@field height number
---@field upTime number
---@field fadeTime number
---@field iconColorA number
---@field iconColorR number
---@field iconColorG number
---@field iconColorB number
---@field textSpacing number

local hudGlobalsStructure = {
    anchor = {type = "word", offset = 0x0},
    x = {type = "word", offset = 0x24},
    y = {type = "word", offset = 0x26},
    width = {type = "float", offset = 0x28},
    height = {type = "float", offset = 0x2C},
    upTime = {type = "float", offset = 0x68},
    fadeTime = {type = "float", offset = 0x6C},
    iconColorA = {type = "float", offset = 0x70},
    iconColorR = {type = "float", offset = 0x74},
    iconColorG = {type = "float", offset = 0x78},
    iconColorB = {type = "float", offset = 0x7C},
    textColorA = {type = "float", offset = 0x80},
    textColorR = {type = "float", offset = 0x84},
    textColorG = {type = "float", offset = 0x88},
    textColorB = {type = "float", offset = 0x8C},
    textSpacing = {type = "float", offset = 0x90}
}

---@class cinematicGlobals
---@field isInProgress boolean
---@field isShowingLetterbox boolean

local cinematicGlobalsStructure = {
    isInProgress = {type = "bit", offset = 0x9, bitLevel = 0},
    isShowingLetterbox = {type = "bit", offset = 0x8, bitLevel = 0}
}

------------------------------------------------------------------------------
-- LuaBlam globals
------------------------------------------------------------------------------

-- Provide with public blam! data tables
blam.addressList = addressList
blam.tagClasses = tagClasses
blam.objectClasses = objectClasses
blam.joystickInputs = joystickInputs
blam.dPadValues = dPadValues
blam.cameraTypes = cameraTypes
blam.consoleColors = consoleColors
blam.netgameFlagClasses = netgameFlagClasses
blam.gameTypeClasses = gameTypeClasses
blam.multiplayerTeamClasses = multiplayerTeamClasses
blam.unitTeamClasses = unitTeamClasses
blam.objectNetworkRoleClasses = objectNetworkRoleClasses

---@class tagDataHeader
---@field array any
---@field scenario string
---@field count number

---@type tagDataHeader
blam.tagDataHeader = createBindTable(addressList.tagDataHeader, tagDataHeaderStructure)

------------------------------------------------------------------------------
-- LuaBlam API
------------------------------------------------------------------------------

-- Add utilities to library
blam.dumpObject = dumpObject
blam.consoleOutput = consoleOutput
blam.null = null

---Get the current game camera type
---@return number?
function blam.getCameraType()
    local camera = read_word(addressList.cameraType)
    if camera then
        if camera == 22192 then
            return cameraTypes.scripted
        elseif camera == 30400 then
            return cameraTypes.firstPerson
        elseif camera == 30704 then
            return cameraTypes.devcam
            -- FIXME Validate this value, it seems to be wrong!
        elseif camera == 21952 then
            return cameraTypes.thirdPerson
        elseif camera == 23776 then
            return cameraTypes.deadCamera
        end
    end
    return nil
end

--- Get input from joystick assigned in the game
---@param joystickOffset number Offset input from the joystick data, use blam.joystickInputs
---@return boolean | number Value of the joystick input
function blam.getJoystickInput(joystickOffset)
    -- Based on aLTis controller method
    -- TODO Check if it is better to return an entire table with all input values 
    joystickOffset = joystickOffset or 0
    -- Nothing is pressed by default
    ---@type boolean | number
    local inputValue = false
    -- Look for every input from every joystick available
    for controllerId = 0, 3 do
        local inputAddress = addressList.joystickInput + controllerId * 0xA0
        if (joystickOffset >= 30 and joystickOffset <= 38) then
            -- Sticks
            inputValue = inputValue + read_long(inputAddress + joystickOffset)
        elseif (joystickOffset > 96) then
            -- D-pad related
            local tempValue = read_word(inputAddress + 96)
            if (tempValue == joystickOffset - 100) then
                inputValue = true
            end
        else
            inputValue = inputValue + read_byte(inputAddress + joystickOffset)
        end
    end
    return inputValue
end

--- Create a tag object from a given address, this object can't write data to game memory
---@param address integer
---@return tag?
function blam.tag(address)
    if (address and address ~= 0) then
        -- Generate a new tag object from class
        local tag = createBindTable(address, tagHeaderStructure)

        -- Get all the tag info
        local tagInfo = dumpObject(tag)

        -- Set up values
        tagInfo.address = address
        tagInfo.path = read_string(tagInfo.path)
        -- TODO Optimize this function
        -- Also review class prop type as it seems we are transforming it to a string but it is 
        -- a number in the binded structure
        tagInfo.class = tagClassFromInt(tagInfo.class --[[@as number]] )

        return tagInfo
    end
    return nil
end

--- Return a tag object given tagPath and tagClass or just tagId
---@param tagIdOrTagPath string | number
---@param tagClass? string
---@return tag?
function blam.getTag(tagIdOrTagPath, tagClass, ...)
    local tagId
    local tagPath

    -- Get arguments from table
    if (isNumber(tagIdOrTagPath)) then
        tagId = tagIdOrTagPath
    elseif (isString(tagIdOrTagPath)) then
        tagPath = tagIdOrTagPath
    elseif (not tagIdOrTagPath) then
        return nil
    end

    if (...) then
        consoleOutput(debug.traceback("Wrong number of arguments on get tag function", 2),
                      consoleColors.error)
    end

    local tagAddress

    -- Get tag address
    if (tagId) then
        if (tagId < 0xFFFF) then
            -- Calculate tag id
            tagId = read_dword(blam.tagDataHeader.array + (tagId * 0x20 + 0xC))
        end
        tagAddress = get_tag(tagId)
    elseif (tagClass and tagPath) then
        tagAddress = get_tag(tagClass, tagPath --[[@as string]] )
    end

    if tagAddress then
        return blam.tag(tagAddress)
    end
end

--- Create a player object given player entry table address
---@param address? number
---@return player?
function blam.player(address)
    if address and isValid(address) then
        return createBindTable(address, playerStructure)
    end
    return nil
end

--- Create a blamObject given address
---@param address? number
---@return blamObject?
function blam.object(address)
    if address and isValid(address) then
        return createBindTable(address, objectStructure)
    end
    return nil
end

--- Create a Projectile object given address
---@param address? number
---@return projectile?
function blam.projectile(address)
    if address and isValid(address) then
        return createBindTable(address, projectileStructure)
    end
    return nil
end

--- Create a Unit object from a given address
---@param address? number
---@return unit?
function blam.unit(address)
    if address and isValid(address) then
        return createBindTable(address, unitStructure)
    end
    return nil
end

--- Create a Biped object from a given address
---@param address? number
---@return biped?
function blam.biped(address)
    if address and isValid(address) then
        return createBindTable(address, bipedStructure)
    end
    return nil
end

--- Create a Vehicle object from a given address
---@param address? number
---@return vehicle?
function blam.vehicle(address)
    if address and isValid(address) then
        return createBindTable(address, vehicleStructure)
    end
    return nil
end

--- Create a biped tag from a tag path or id
---@param tag string | number
---@return bipedTag?
function blam.bipedTag(tag)
    if isValid(tag) then
        local bipedTag = blam.getTag(tag, tagClasses.biped)
        if (bipedTag) then
            return createBindTable(bipedTag.data, bipedTagStructure)
        end
    end
    return nil
end

--- Create a Unicode String List object from a tag path or id
---@param tag string | number
---@return unicodeStringList?
function blam.unicodeStringList(tag)
    if isValid(tag) then
        local unicodeStringListTag = blam.getTag(tag, tagClasses.unicodeStringList)
        if (unicodeStringListTag) then
            return createBindTable(unicodeStringListTag.data, unicodeStringListStructure)
        end
    end
    return nil
end

--- Create a bitmap object from a tag path or id
---@param tag string | number
---@return bitmap?
function blam.bitmap(tag)
    if isValid(tag) then
        local bitmapTag = blam.getTag(tag, tagClasses.bitmap)
        if (bitmapTag) then
            return createBindTable(bitmapTag.data, bitmapStructure)
        end
    end
end

--- Create a UI Widget Definition object from a tag path or id
---@param tag string | number
---@return uiWidgetDefinition?
function blam.uiWidgetDefinition(tag)
    if isValid(tag) then
        local uiWidgetDefinitionTag = blam.getTag(tag, tagClasses.uiWidgetDefinition)
        if (uiWidgetDefinitionTag) then
            return createBindTable(uiWidgetDefinitionTag.data, uiWidgetDefinitionStructure)
        end
    end
    return nil
end

--- Create a UI Widget Collection object from a tag path or id
---@param tag string | number
---@return uiWidgetCollection?
function blam.uiWidgetCollection(tag)
    if isValid(tag) then
        local uiWidgetCollectionTag = blam.getTag(tag, tagClasses.uiWidgetCollection)
        if (uiWidgetCollectionTag) then
            return createBindTable(uiWidgetCollectionTag.data, uiWidgetCollectionStructure)
        end
    end
    return nil
end

--- Create a Tag Collection object from a tag path or id
---@param tag string | number
---@return tagCollection?
function blam.tagCollection(tag)
    if isValid(tag) then
        local tagCollectionTag = blam.getTag(tag, tagClasses.tagCollection)
        if (tagCollectionTag) then
            return createBindTable(tagCollectionTag.data, tagCollectionStructure)
        end
    end
    return nil
end

--- Create a Weapon HUD Interface object from a tag path or id
---@param tag string | number
---@return weaponHudInterface?
function blam.weaponHudInterface(tag)
    if isValid(tag) then
        local weaponHudInterfaceTag = blam.getTag(tag, tagClasses.weaponHudInterface)
        if (weaponHudInterfaceTag) then
            return createBindTable(weaponHudInterfaceTag.data, weaponHudInterfaceStructure)
        end
    end
    return nil
end

--- Create a Scenario object from a tag path or id
---@param tag? string | number
---@return scenario?
function blam.scenario(tag)
    local scenarioTag = blam.getTag(tag or 0, tagClasses.scenario)
    if (scenarioTag) then
        return createBindTable(scenarioTag.data, scenarioStructure)
    end
end

--- Create a Scenery object from a tag path or id
---@param tag string | number
---@return scenery?
function blam.scenery(tag)
    if isValid(tag) then
        local sceneryTag = blam.getTag(tag, tagClasses.scenery)
        if (sceneryTag) then
            return createBindTable(sceneryTag.data, sceneryStructure)
        end
    end
    return nil
end

--- Create a Collision Geometry object from a tag path or id
---@param tag string | number
---@return collisionGeometry?
function blam.collisionGeometry(tag)
    if isValid(tag) then
        local collisionGeometryTag = blam.getTag(tag, tagClasses.collisionGeometry)
        if (collisionGeometryTag) then
            return createBindTable(collisionGeometryTag.data, collisionGeometryStructure)
        end
    end
    return nil
end

--- Create a Model Animations object from a tag path or id
---@param tag string | number
---@return modelAnimations?
function blam.modelAnimations(tag)
    if isValid(tag) then
        local modelAnimationsTag = blam.getTag(tag, tagClasses.modelAnimations)
        if (modelAnimationsTag) then
            return createBindTable(modelAnimationsTag.data, modelAnimationsStructure)
        end
    end
    return nil
end

--- Create a Weapon object from the given object address
---@param address? number
---@return weapon?
function blam.weapon(address)
    if address and isValid(address) then
        return createBindTable(address, weaponStructure)
    end
    return nil
end

--- Create a Weapon tag object from a tag path or id
---@param tag string | number
---@return weaponTag?
function blam.weaponTag(tag)
    if isValid(tag) then
        local weaponTag = blam.getTag(tag, tagClasses.weapon)
        if (weaponTag) then
            return createBindTable(weaponTag.data, weaponTagStructure)
        end
    end
    return nil
end

--- Create a model (gbxmodel) object from a tag path or id
---@param tag string | number
---@return gbxModel?
function blam.model(tag)
    if isValid(tag) then
        local modelTag = blam.getTag(tag, tagClasses.model)
        if (modelTag) then
            return createBindTable(modelTag.data, modelStructure)
        end
    end
    return nil
end
-- Alias
blam.gbxmodel = blam.model

--- Create a Globals tag object from a tag path or id, default globals path by default
---@param tag? string | number
---@return globalsTag?
function blam.globalsTag(tag)
    local tag = tag or "globals\\globals"
    if isValid(tag) then
        local globalsTag = blam.getTag(tag, tagClasses.globals)
        if (globalsTag) then
            return createBindTable(globalsTag.data, globalsTagStructure)
        end
    end
    return nil
end

--- Create a First person object from a given address, game known address by default
---@param address? number
---@return firstPerson
function blam.firstPerson(address)
    return createBindTable(address or addressList.firstPerson, firstPersonStructure)
end

--- Create a Device Machine object from a given address
---@param address? number
---@return deviceMachine?
function blam.deviceMachine(address)
    if address and isValid(address) then
        return createBindTable(address, deviceMachineStructure)
    end
    return nil
end

--- Create a HUD Globals tag object from a given address
---@param tag string | number
---@return hudGlobals?
function blam.hudGlobals(tag)
    if isValid(tag) then
        local hudGlobals = blam.getTag(tag, tagClasses.hudGlobals)
        if (hudGlobals) then
            return createBindTable(hudGlobals.data, hudGlobalsStructure)
        end
    end
    return nil
end

--- Return a blam object given object index or id.
--- Also returns objectId when given an object index.
---@param idOrIndex number
---@return blamObject?, number?
function blam.getObject(idOrIndex)
    local objectId
    local objectAddress

    -- Get object address
    if (idOrIndex) then
        -- Get object ID
        if (idOrIndex < 0xFFFF) then
            local index = idOrIndex

            -- Get objects table
            local table = createBindTable(addressList.objectTable, dataTableStructure)
            if (index > table.capacity) then
                return nil
            end

            -- Calculate object ID (this may be invalid, be careful)
            objectId =
                (read_word(table.firstElementAddress + index * table.elementSize) * 0x10000) + index
        else
            objectId = idOrIndex
        end

        objectAddress = get_object(objectId)

        if objectAddress then
            return blam.object(objectAddress), objectId
        end
    end
    return nil
end

--- Return an element from the device machines table
---@param index number
---@return number?
function blam.getDeviceGroup(index)
    -- Get object address
    if index then
        -- Get objects table
        local table = createBindTable(read_dword(addressList.deviceGroupsTable),
                                      deviceGroupsTableStructure)
        -- Calculate object ID (this may be invalid, be careful)
        local itemOffset = table.elementSize * index
        local item = read_float(table.firstElementAddress + itemOffset + 0x4)
        return item
    end
    return nil
end

local syncedObjectsTable = {
    maximumObjectsCount = {type = "dword", offset = 0x0},
    initialized = {type = "byte", offset = 0xC},
    objectsCount = {type = "dword", offset = 0x18},
    firstElementAddress = {type = "dword", offset = 0x28}
}

local function getSyncedObjectsTable()
    local tableAddress
    if blam.isGameSAPP() then
        tableAddress = addressList.syncedNetworkObjects
    else
        tableAddress = read_dword(addressList.syncedNetworkObjects)
        if tableAddress == 0 then
            console_out("Synced objects table is not accesible yet.")
            return nil
        end
    end

    return createBindTable(tableAddress, syncedObjectsTable)
end

--- Return the maximum allowed network objects count
---@return number
function blam.getMaximumNetworkObjects()
    local syncedObjectsTable = getSyncedObjectsTable()
    if not syncedObjectsTable then
        return engineConstants.defaultNetworkObjectsCount
    end

    -- For some reason fist element entry is always used, so we need to substract 1
    return syncedObjectsTable.maximumObjectsCount - 1
end

--- Return an element from the synced objects table
---@param index number
---@return number?
function blam.getObjectIdBySyncedIndex(index)
    if index then
        local syncedObjectsTable = getSyncedObjectsTable()
        if not syncedObjectsTable then
            return nil
        end

        if syncedObjectsTable.objectsCount == 0 then
            return nil
        end
        if not syncedObjectsTable.initialized == 1 then
            return nil
        end
        -- For some reason fist element entry is always used, so we need to substract 1
        if index >= syncedObjectsTable.maximumObjectsCount - 1 then
            return nil
        end

        local entryOffset = 4 * index
        -- Ignore first entry, it's always used so add 4 bytes offset
        local entryAddress = syncedObjectsTable.firstElementAddress + entryOffset + 0x4
        local objectId = read_dword(entryAddress)
        if blam.isNull(objectId) then
            return nil
        end
        return objectId
    end
    return nil
end

---@class blamRequest
---@field requestString string
---@field timeout number
---@field callback function<boolean, string>
---@field sentAt number

local rconEvents = {}
local maxRconDataLength = 60

blam.rcon = {}

---Define a request event callback
---@param eventName string
---@param callback fun(message?: string, playerIndex?: number): string?
function blam.rcon.event(eventName, callback)
    rconEvents[eventName:lower()] = callback
end

---Dispatch an rcon event to a client or server trough rcon.
---
--- As a client, you can only send messages to the server.
---
--- As a server, you can send messages to a specific client or all clients.
---@param eventName string Path or name of the resource we want to get
---@param message? string Message to send to the server
---@param playerIndex? number Player index to send the message to
---@overload fun(eventName: string, playerIndex: number)
---@return {callback: fun(callback: fun(response: string, playerIndex?: number))}
function blam.rcon.dispatch(eventName, message, playerIndex)
    -- if server_type ~= "dedicated" then
    --    console_out("Warning, requests only work while connected to a dedicated server.")
    -- end
    assert(eventName ~= nil, "Event must not be empty")
    assert(type(eventName) == "string", "Event must be a string")
    local message = message
    local playerIndex = playerIndex
    if message and type(message) == "number" then
        playerIndex = message
        message = nil
    end
    if eventName then
        if blam.isGameSAPP() then
            if playerIndex then
                rprint(playerIndex, ("?%s?%s"):format(eventName, message))
            else
                for i = 1, 16 do
                    rprint(i, ("?%s?%s"):format(eventName, message))
                end
            end
        else
            local request = ("?%s?%s"):format(eventName, message)
            assert(#request <= maxRconDataLength, "Rcon request is too long")
            if blam.isGameDedicated() then
                execute_script("rcon blam " .. request)
            else
                blam.rcon.handle(request)
            end
        end
        return {
            callback = function()
                blam.rcon.event(eventName .. "+", callback)
            end
        }
    end
    error("No event name provided")
end

---Evaluate rcon event and handle it as a request
---@param data string
---@param password? string
---@param playerIndex? number
---@return boolean | nil
function blam.rcon.handle(data, password, playerIndex)
    if data:sub(1, 1) == "?" then
        if blam.isGameSAPP() then
            if password ~= "blam" then
                return nil
            end
        end
        local data = split(data, "?")
        local eventName = data[2]
        local message = data[3]
        local event = rconEvents[eventName:lower()]
        if event then
            local response = event(message, playerIndex)
            if response then
                if blam.isGameSAPP() then
                    rprint(playerIndex, response)
                else
                    execute_script(("rcon blam ?%s?%s"):format(eventName .. "+", response))
                end
            end
            return false
        else
            error("No rcon event handler for " .. eventName)
        end
    end
    -- Pass request to the server
    return nil
end

local passwordAddress
local failMessageAddress

---Patch rcon server function to avoid failed rcon messages
function blam.rcon.patch()
    passwordAddress = read_dword(sig_scan("7740BA??????008D9B000000008A01") + 0x3)
    failMessageAddress = read_dword(sig_scan("B8????????E8??000000A1????????55") + 0x1)
    if passwordAddress and failMessageAddress then
        -- Remove "rcon command failure" message
        safe_write(true)
        write_byte(failMessageAddress, 0x0)
        safe_write(false)
        -- Read current rcon in the server
        local serverRcon = read_string(passwordAddress)
        if serverRcon then
            console_out("Server rcon password is: \"" .. serverRcon .. "\"")
        else
            console_out("Error, at getting server rcon, please set and enable rcon on the server.")
        end
    else
        console_out("Error, at obtaining rcon patches, please check SAPP version.")
    end
end

---Unpatch rcon server function to restore failed rcon messages
function blam.rcon.unpatch()
    if failMessageAddress then
        -- Restore "rcon command failure" message
        safe_write(true)
        write_byte(failMessageAddress, 0x72)
        safe_write(false)
    end
end

--- Find the path, index and id of a tag given partial tag path and tag type
---@param partialTagPath string
---@param searchTagType string
---@return tag? tag
function blam.findTag(partialTagPath, searchTagType)
    for tagIndex = 0, blam.tagDataHeader.count - 1 do
        local tag = blam.getTag(tagIndex)
        if (tag and tag.path:find(partialTagPath, 1, true) and tag.class == searchTagType) then
            return tag
        end
    end
    return nil
end

--- Find the path, index and id of a list of tags given partial tag path and tag type
---@param partialTagPath string
---@param searchTagType string
---@return tag[] tag
function blam.findTagsList(partialTagPath, searchTagType)
    local tagsList
    for tagIndex = 0, blam.tagDataHeader.count - 1 do
        local tag = blam.getTag(tagIndex)
        if (tag and tag.path:find(partialTagPath, 1, true) and tag.class == searchTagType) then
            if not tagsList then
                tagsList = {}
            end
            tagsList[#tagsList + 1] = tag
        end
    end
    return tagsList
end

--- Return the index of an id number
---@param id number
function blam.getIndexById(id)
    if id then
        return fmod(id, 0x10000)
    end
    return nil
end

---@class vector2D
---@field x number
---@field y number

---@class vector3D
---@field x number
---@field y number
---@field z number

---@class vector4D
---@field x number
---@field y number
---@field z number
---@field w number

---Returns game rotation vectors from euler angles, return optional rotation matrix, based on
---[source.](https://www.mecademic.com/en/how-is-orientation-in-space-represented-with-euler-angles)
--- @param yaw number
--- @param pitch number
--- @param roll number
--- @return vector3D, vector3D
local function eulerAnglesToVectors(yaw, pitch, roll)
    local yaw = rad(yaw)
    local pitch = rad(-pitch) -- Negative pitch due to Sapien handling anticlockwise pitch
    local roll = rad(roll)
    local matrix = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}

    -- Roll, Pitch, Yaw = a, b, y
    local cosA = cos(roll)
    local sinA = sin(roll)
    local cosB = cos(pitch)
    local sinB = sin(pitch)
    local cosY = cos(yaw)
    local sinY = sin(yaw)

    matrix[1][1] = cosB * cosY
    matrix[1][2] = -cosB * sinY
    matrix[1][3] = sinB
    matrix[2][1] = cosA * sinY + sinA * sinB * cosY
    matrix[2][2] = cosA * cosY - sinA * sinB * sinY
    matrix[2][3] = -sinA * cosB
    matrix[3][1] = sinA * sinY - cosA * sinB * cosY
    matrix[3][2] = sinA * cosY + cosA * sinB * sinY
    matrix[3][3] = cosA * cosB

    local v1 = {x = matrix[1][1], y = matrix[2][1], z = matrix[3][1]}
    local v2 = {x = matrix[1][3], y = matrix[2][3], z = matrix[3][3]}

    return v1, v2
end

--- Get euler angles rotation from game rotation vectors
--- @param v1 vector3D Vector with first column values from rotation matrix
--- @param v2 vector3D Vector with third column values from rotation matrix
--- @return number yaw, number pitch, number roll
local function vectorsToEulerAngles(v1, v2)
    local v3 = {
        x = v1.y * v2.z - v1.z * v2.y,
        y = v1.z * v2.x - v1.x * v2.z,
        z = v1.x * v2.y - v1.y * v2.x
    }

    local matrix = {{v1.x, v3.x, v2.x}, {v1.y, v3.y, v2.y}, {v1.z, v3.z, v2.z}}

    -- Extract individual matrix elements
    local m11, m12, m13 = matrix[1][1], matrix[1][2], matrix[1][3]
    local m21, m22, m23 = matrix[2][1], matrix[2][2], matrix[2][3]
    local m31, m32, m33 = matrix[3][1], matrix[3][2], matrix[3][3]

    -- Calculate yaw (heading) angle
    local yaw = atan2(m12, m11)

    -- Calculate pitch (attitude) angle
    local pitch = atan2(-m13, sqrt(m23 ^ 2 + m33 ^ 2))

    -- Calculate roll (bank) angle
    local roll = -atan2(m23, m33)

    -- Convert angles from radians to degrees
    yaw = deg(yaw)
    pitch = deg(pitch)
    roll = deg(roll)

    -- Adjust angles to the range [0, 359]
    yaw = fmod(yaw + 360, 360)
    pitch = fmod(pitch + 360, 360)
    roll = fmod(roll + 360, 360)

    return yaw, pitch, roll
end

--- Get rotation angles from game object
---
--- Assuming clockwise rotation and absolute angles from 0 to 360
---@param object blamObject
---@return number yaw, number pitch, number roll
function blam.getObjectRotation(object)
    local v1 = {x = object.vX, y = object.vY, z = object.vZ}
    local v2 = {x = object.v2X, y = object.v2Y, z = object.v2Z}
    return vectorsToEulerAngles(v1, v2)
end

--- Get rotation angles from game vectors
---
--- Assuming clockwise rotation and absolute angles from 0 to 360
---@param v1 vector3D
---@param v2 vector3D
---@return number yaw, number pitch, number roll
function blam.getVectorRotation(v1, v2)
    return vectorsToEulerAngles(v1, v2)
end

--- Rotate object into desired angles
---
--- Assuming clockwise rotation and absolute angles from 0 to 360
---@param object blamObject
---@param yaw number
---@param pitch number
---@param roll number
function blam.rotateObject(object, yaw, pitch, roll)
    local v1, v2 = eulerAnglesToVectors(yaw, pitch, roll)
    object.vX = v1.x
    object.vY = v1.y
    object.vZ = v1.z
    object.v2X = v2.x
    object.v2Y = v2.y
    object.v2Z = v2.z
end

--- Get screen resolution
---@return {width: number, height: number, aspectRatio: number}
function blam.getScreenData()
    local height = read_word(addressList.screenResolution)
    local width = read_word(addressList.screenResolution + 0x2)
    return {width = width, height = height, aspectRatio = width / height}
end

--- Get the current game state
---@return {isLayerOpened: boolean, isGamePaused: boolean}
function blam.getGameState()
    return {
        isLayerOpened = read_byte(addressList.gameOnMenus) == 0,
        isGamePaused = read_byte(addressList.gamePaused) == 0
    }
end

--- Get object absolute coordinates
---Returns the absolute coordinates of an object, considering parent object coordinates if any.
---@param object blamObject
---@return vector3D
function blam.getAbsoluteObjectCoordinates(object)
    local coordinates = {x = object.x, y = object.y, z = object.z}
    if not isNull(object.parentObjectId) then
        local parentObject = blam.object(get_object(object.parentObjectId))
        if parentObject then
            coordinates.x = coordinates.x + parentObject.x
            coordinates.y = coordinates.y + parentObject.y
            coordinates.z = coordinates.z + parentObject.z
        end
    end
    return coordinates
end

--- Returns binded table to game cinematic globals
---@return cinematicGlobals
function blam.cinematicGlobals()
    return createBindTable(read_dword(addressList.cinematicGlobals), cinematicGlobalsStructure)
end

return blam

end,

["luna"] = function()
--------------------
-- Module: 'luna'
--------------------
local luna = {_VERSION = "2.0.1"}

luna.string = {}

--- Split a string into a table of substrings by `sep`, or by each character if `sep` is not provided.
---@param s string
---@param sep? string
---@return string[]
---@nodiscard
function string.split(s, sep)
    assert(s ~= nil, "string.split: s must not be nil")
    local elements = {}
    -- Support splitting by any character or string.
    if sep == nil or sep == "" then
        for i = 1, #s do
            elements[i] = s:sub(i, i)
        end
    else
        -- Avoid using a pattern
        local position = 0
        for st, sp in function()
            return s:find(sep, position, true)
        end do
            table.insert(elements, s:sub(position, st - 1))
            position = sp + 1
        end
        table.insert(elements, s:sub(position))
    end
    return elements
end

--- Return a string with all leading whitespace removed.
---@param s string
---@return string
---@nodiscard
function string.ltrim(s)
    assert(s ~= nil, "string.ltrim: s must not be nil")
    return s:match "^%s*(.-)$"
end

--- Return a string with all trailing whitespace removed.
---@param s string
---@return string
---@nodiscard
function string.rtrim(s)
    assert(s ~= nil, "string.rtrim: s must not be nil")
    return s:match "^(.-)%s*$"
end

--- Return a string with all leading and trailing whitespace removed.
---@param s string
---@return string
---@nodiscard
function string.trim(s)
    assert(s ~= nil, "string.trim: s must not be nil")
    -- return s:match "^%s*(.-)%s*$"
    return string.ltrim(string.rtrim(s))
end

--- Return a string with all ocurrences of `pattern` replaced with `replacement`.
---
--- **NOTE**: Pattern is a plain string, not a Lua pattern. Use `string.gsub` for Lua patterns.
---@param s string
---@param pattern string
---@param replacement string
---@return string
---@nodiscard
function string.replace(s, pattern, replacement)
    assert(s ~= nil, "string.replace: s must not be nil")
    assert(pattern ~= nil, "string.replace: pattern must not be nil")
    assert(replacement ~= nil, "string.replace: replacement must not be nil")
    local pattern = pattern:escapep()
    local replaced, _ = s:gsub(pattern, replacement:escapep())
    return replaced
end

--- Return a hex encoded string.
---@param s string | number
---@return string
---@nodiscard
function string.tohex(s)
    assert(s ~= nil, "string.hex: s must not be nil")
    if type(s) == "number" then
        return string.format("%08.8x", s)
    end
    return (s:gsub(".", function(c)
        return string.format("%02x", string.byte(c))
    end))
end

--- Return a hex decoded string.
---@param s string
---@return string
---@nodiscard
function string.fromhex(s)
    assert(s ~= nil, "string.fromhex: s must not be nil")
    return (s:gsub("..", function(cc)
        return string.char(tonumber(cc, 16))
    end))
end

--- Resturn if a string starts with a given substring.
---@param s string
---@param start string
---@return boolean
---@nodiscard
function string.startswith(s, start)
    assert(s ~= nil, "string.startswith: s must not be nil")
    assert(start ~= nil, "string.startswith: start must not be nil")
    return string.sub(s, 1, string.len(start)) == start
end

--- Resturn if a string ends with a given substring.
---@param s string
---@param ending string
---@return boolean
---@nodiscard
function string.endswith(s, ending)
    assert(s ~= nil, "string.endswith: s must not be nil")
    assert(ending ~= nil, "string.endswith: ending must not be nil")
    return ending == "" or string.sub(s, -string.len(ending)) == ending
end

--- Return a string with template variables replaced with values from a table.
---@param s string
---@param t table<string, string | number | boolean>
---@return string
---@nodiscard
function string.template(s, t)
    assert(s ~= nil, "string.template: s must not be nil")
    assert(t ~= nil, "string.template: t must not be nil")
    return (s:gsub("{(.-)}", function(k)
        return t[k] or ""
    end))
end

--- Return if a string includes a given substring.
---@param s string
---@param substring string
---@return boolean
---@nodiscard
function string.includes(s, substring)
    assert(s ~= nil, "string.includes: s must not be nil")
    assert(substring ~= nil, "string.includes: substring must not be nil")
    return string.find(s, substring, 1, true) ~= nil
end

--- Return a string with all lua pattern characters escaped.
---@param s string
---@return string
---@nodiscard
function string.escapep(s)
    assert(s ~= nil, "string.escape: s must not be nil")
    return (s:gsub("%%", "%%%%"):gsub("%z", "%%z"):gsub("([%^%$%(%)%.%[%]%*%+%-%?])", "%%%1"))
end

luna.string.split = string.split
luna.string.ltrim = string.ltrim
luna.string.rtrim = string.rtrim
luna.string.trim = string.trim
luna.string.replace = string.replace
luna.string.tohex = string.tohex
luna.string.fromhex = string.fromhex
luna.string.startswith = string.startswith
luna.string.endswith = string.endswith
luna.string.template = string.template
luna.string.includes = string.includes
luna.string.escapep = string.escapep

luna.table = {}

--- Return a deep copy of a table.
---
--- If the table contains other tables, they are copied as well, even metatables.
---@generic T
---@param t T
---@return T
---@nodiscard
function table.copy(t)
    assert(t ~= nil, "table.copy: t must not be nil")
    assert(type(t) == "table", "table.copy: t must be a table")
    local u = {}
    for k, v in pairs(t) do
        u[k] = type(v) == "table" and table.copy(v) or v
    end
    return setmetatable(u, getmetatable(t))
end

--- Find and return first index of `value` in `t`.
---@generic V
---@param t table<number, V>: { [number]: V }
---@param value V
---@return number?
---@nodiscard
function table.indexof(t, value)
    assert(t ~= nil, "table.find: t must not be nil")
    assert(type(t) == "table", "table.find: t must be a table")
    for i, v in ipairs(t) do
        if v == value then
            return i
        end
    end
end

--- Return a table with all keys and values swapped.
---@generic K, V
---@param t table<K, V>
---@return {[V]: K}
---@nodiscard
function table.flip(t)
    assert(t ~= nil, "table.flip: t must not be nil")
    assert(type(t) == "table", "table.flip: t must be a table")
    local u = {}
    for k, v in pairs(t) do
        u[v] = k
    end
    return u
end

--- Returns the first element of `t` that satisfies the predicate `f`.
---@generic K, V
---@param t table<K, V>
---@param f fun(v: V, k: K): boolean
---@return V?
---@nodiscard
function table.find(t, f)
    assert(t ~= nil, "table.find: t must not be nil")
    assert(type(t) == "table", "table.find: t must be a table")
    assert(f ~= nil, "table.find: f must not be nil")
    assert(type(f) == "function", "table.find: f must be a function")
    for k, v in pairs(t) do
        if f(v, k) then
            return v
        end
    end
end

--- Returns an array of all keys in `t`.
---@generic K, V
---@param t table<K, V>
---@return K[]
---@nodiscard
function table.keys(t)
    assert(t ~= nil, "table.keys: t must not be nil")
    assert(type(t) == "table", "table.keys: t must be a table")
    local keys = {}
    for k in pairs(t) do
        keys[#keys + 1] = k
    end
    return keys
end

--- Returns an array of all values in `t`.
---@generic K, V
---@param t table<K, V>
---@return V[]
---@nodiscard
function table.values(t)
    assert(t ~= nil, "table.values: t must not be nil")
    assert(type(t) == "table", "table.values: t must be a table")
    local values = {}
    for _, v in pairs(t) do
        values[#values + 1] = v
    end
    return values
end

--- Returns a table with all elements of `t` that satisfy the predicate `f`.
---@generic K, V
---@param t table<K, V>
---@param f fun(v: V, k: K): boolean
---@return {[K]: V}
---@nodiscard
function table.filter(t, f)
    assert(t ~= nil, "table.filter: t must not be nil")
    assert(type(t) == "table", "table.filter: t must be a table")
    assert(f ~= nil, "table.filter: f must not be nil")
    assert(type(f) == "function", "table.filter: f must be a function")
    local filtered = {}
    for k, v in pairs(t) do
        if f(v, k) then
            filtered[#filtered + 1] = v
        end
    end
    return filtered
end

--- Returns a table with all elements of `t` that satisfy the predicate `f`.
---
--- **NOTE**: It keeps original keys in the new table.
---@generic K, V
---@param t table<K, V>
---@param f fun(v: V, k: K): boolean
---@return {[K]: V}
---@nodiscard
function table.kfilter(t, f)
    assert(t ~= nil, "table.kfilter: t must not be nil")
    assert(type(t) == "table", "table.kfilter: t must be a table")
    assert(f ~= nil, "table.kfilter: f must not be nil")
    assert(type(f) == "function", "table.kfilter: f must be a function")
    local filtered = {}
    for k, v in pairs(t) do
        if f(v, k) then
            filtered[k] = v
        end
    end
    return filtered
end

--- Returns a table with all elements of `t` mapped by function `f`.
---
--- **NOTE**: It keeps original keys in the new table.
---@generic K, V
---@generic R
---@param t table<K, V>
---@param f fun(v: V, k: K): R
---@return {[K]: R}
--@return R[]
---@nodiscard
function table.map(t, f)
    assert(t ~= nil, "table.map: t must not be nil")
    assert(type(t) == "table", "table.map: t must be a table")
    assert(f ~= nil, "table.map: f must not be nil")
    assert(type(f) == "function", "table.map: f must be a function")
    local mapped = {}
    for k, v in pairs(t) do
        mapped[k] = f(v, k)
    end
    return mapped
end

--- Returns a table merged from all tables passed as arguments.
---@generic K, V
---@vararg table<K, V>
---@return {[K]: V}
---@nodiscard
function table.merge(...)
    local merged = {}
    for _, t in ipairs {...} do
        for k, v in pairs(t) do
            merged[k] = v
        end
    end
    return merged
end

--- Returns a table with all elements in reversed order.
---@generic T
---@param t T
---@return T
---@nodiscard
function table.reverse(t)
    assert(t ~= nil, "table.reverse: t must not be nil")
    assert(type(t) == "table", "table.reverse: t must be a table")
    local reversed = {}
    for i = #t, 1, -1 do
        reversed[#reversed + 1] = t[i]
    end
    return reversed
end

--- Return a slice of a table, from `start` to `stop`
---
--- If `stop` is not provided, it will slice to the end of the table.
---@generic K, V
---@param t table<K, V>
---@param start number Index to start slice from.
---@param stop? number Index to stop slice at.
---@return {[K]: V}
---@nodiscard
function table.slice(t, start, stop)
    assert(t ~= nil, "table.slice: t must not be nil")
    assert(type(t) == "table", "table.slice: t must be a table")
    assert(start ~= nil, "table.slice: start must not be nil")
    assert(type(start) == "number", "table.slice: start must be a number")
    if stop then
        assert(type(stop) == "number", "table.slice: stop must be a number")
    end
    local sliced = {}
    for i = start, stop or #t do
        sliced[#sliced + 1] = t[i]
    end
    return sliced
end

--- Return an array of chunks from a table, each chunk containing `size` elements.
---
--- If `t` is not evenly divisible by `size`, last chunk will contain the remaining elements.
---@generic K, V
---@param t table<K, V>
---@param size number
---@return {[K]: V[]}
---@nodiscard
function table.chunks(t, size)
    assert(t ~= nil, "table.chunks: t must not be nil")
    assert(type(t) == "table", "table.chunks: t must be a table")
    assert(size ~= nil, "table.chunks: size must not be nil")
    assert(type(size) == "number", "table.chunks: size must be a number")
    local chunks = {}
    for i = 1, #t, size do
        chunks[#chunks + 1] = table.slice(t, i, i + size - 1)
    end
    return chunks
end

luna.table.copy = table.copy
luna.table.indexof = table.indexof
luna.table.flip = table.flip
luna.table.find = table.find
luna.table.keys = table.keys
luna.table.values = table.values
luna.table.filter = table.filter
luna.table.map = table.map
luna.table.merge = table.merge
luna.table.reverse = table.reverse
luna.table.slice = table.slice
luna.table.chunks = table.chunks

luna.file = {}

--- Read a file into a string.
---@param path string
---@return string?
---@nodiscard
function luna.file.read(path)
    assert(path ~= nil, "file.read: path must not be nil")
    local file = io.open(path, "r")
    if file then
        local content = file:read "*a"
        file:close()
        return content
    end
end

--- Write a file from a string.
---@param path string
---@param content string
---@return boolean
function luna.file.write(path, content)
    assert(path ~= nil, "file.write: path must not be nil")
    assert(content ~= nil, "file.write: content must not be nil")
    local file = io.open(path, "w")
    if file then
        file:write(content)
        file:close()
        return true
    end
    return false
end

--- Append a string to a file.
---@param path string
---@param content string
---@return boolean
function luna.file.append(path, content)
    assert(path ~= nil, "file.append: path must not be nil")
    assert(content ~= nil, "file.append: content must not be nil")
    local file = io.open(path, "a")
    if file then
        file:write(content)
        file:close()
        return true
    end
    return false
end

--- Return if a file exists.
---@param path string
---@return boolean
---@nodiscard
function luna.file.exists(path)
    assert(path ~= nil, "file.exists: path must not be nil")
    local file = io.open(path, "r")
    if file then
        file:close()
        return true
    end
    return false
end

--- Attempt to remove a file.
---@param path string
---@return boolean
function luna.file.remove(path)
    assert(path ~= nil, "file.remove: path must not be nil")
    return os.remove(path)
end

--- Write file from a byte array.
---@param path string
---@param bytes number[]
---@return boolean
function luna.file.frombytes(path, bytes)
    assert(path ~= nil, "file.frombytes: path must not be nil")
    assert(bytes ~= nil, "file.frombytes: bytes must not be nil")
    local file = io.open(path, "wb")
    if file then
        file:write(string.char(table.unpack(bytes)))
        file:close()
        return true
    end
    return false
end

--- Return a byte array from a file.
---@param path string
---@return number[]?
---@nodiscard
function luna.file.tobytes(path)
    assert(path ~= nil, "file.tobytes: path must not be nil")
    local file = io.open(path, "rb")
    if file then
        local bytes = {string.byte(file:read "*a", 1, -1)}
        file:close()
        return bytes
    end
end

--- Return a boolean from `v` if it is a boolean like value.
---@param v string | boolean | number
---@return boolean
function luna.bool(v)
    assert(v ~= nil, "bool: v must not be nil")
    return v == true or v == "true" or v == 1 or v == "1"
end

--- Return an integer from `v` if possible.
---
--- If `v` is not a number, it will return `fail`.
---@param v string
---@return integer
function tointeger(v)
    assert(v ~= nil, "int: v must not be nil")
    return tonumber(v, 10)
end
luna.int = tointeger

return luna
end,

["the_flood.constants"] = function()
--------------------
-- Module: 'the_flood.constants'
--------------------
-- Lua libraries
local blam = require "blam"
local tagClasses = blam.tagClasses
local findTag = blam.findTag

local constants = {}

constants.color = {
    ---SULFUR---
    cookies_and_cream = "#d9e1ac",
    misty_moss = "#b3b873",
    oxley = "#74a071",
    mustard_green = "#62703c",
    soldier__green = "#505929",
    ---SILVER---
    opal_white = "#a4c0c1", "best option",
    weldon_blue = "#83a1a6",
    steel_teal = "#578a92", "best option",
    dark_electric_blue ="#507178",
    ---BLUE---
    jordi_blue = "#8bb2ff",
    catalina = "#142d75",
    ---GREEN---
    russian_green = "#77935d",
    japanese_laurel = "#3b701d",
    lincoln_green = "#225004",
    ---PURPLE---
    waterloo = "#82839a",
    indigo = "#261e7b",
    ---GOLDEN---
    sahara = "#b1af14",
    verdun = "#5c4d00",
    ---RED---
    copper ="#9d646b",
    dark_tan = "#6a0d0f",
    tamarind = "#341722",
    ---ORANGE---
    bell = "#e38e15",
    fire = "#a74500",
    beech = "#7e3000",
    ---BLACK---
    space ="#273132",
    black = "#000000",
    ---WHITE---
    white = "#FFFFFF",
    mist = "##a5c5cb",
    ---CYANOTIC---
    mint = "#96edca",
    mulled_wine = "#473e5a",
    ---NIGHTFALL---
    keppel ="#37979d",
    eden ="#125962",
    elephant = "#0c3739",
    ---MATRIX---
    blue_chill = "#147d97",
    meadow = "#1ca569",
    ---SUNSHINE---
    jellyfish = "#5bc4b2",
    camel = "#c2a54d",
    ---CHIMERA---
    blueberry = "#3c379a",
    valentine = "#e35a58",
    ---IRIDESCENT---
    rose = "#fa51aa",
    bush = "#633da9",
    tealish = "#a6b5eb",
    ---COALESCENCE---
    american_blue = "#3e2f76",
    crayola = "#eb1e49",
    berry = "#a01c4b",
}

-- Constant core values
constants.localPlayerAddress = 0x815918

-- Constant gameplay values
constants.healthRegenerationAmount = 0.0037
-- health recharged on 90 ticks or 3 seconds
constants.healthRegenAiAmount = 0.02
constants.raycastOffset = 0.3
constants.raycastVelocity = 80


-- hsc constants
constants.hsc = {
    playSound = [[(begin (sound_impulse_start "%s" (list_get (players) %s) %s))]]
}

-- Sound References
constants.sounds = {
    uiFGrenadePath = blam.findTag("001_frag_grenade", tagClasses.sound).path,
    uiPGrenadePath = blam.findTag("001_plasma_grenade", tagClasses.sound).path,
    humanRifleZoomIn = blam.findTag("007_human_rifle_zoom_in", tagClasses.sound).path,
    humanRifleZoomOut = blam.findTag("007_human_rifle_zoom_out", tagClasses.sound).path
}

-- Projectile References
constants.projectiles = {
    raycastTag = blam.findTag("raycast", tagClasses.projectile)
}

-- Weapon References
constants.weapons = {
    ma38Tag = blam.findTag("assault_rifle_ma38", tagClasses.weapon)
}

-- Biped References
constants.bipeds = {
    odstAllyTag = blam.findTag("gridharvolur_ally", tagClasses.biped)
}

-- Weapon HUD References
constants.weaponHudInterfaces = {
    ma38HudTag = blam.findTag("assault_rifle_ma38", tagClasses.weaponHudInterface)
}

function constants.get()
    local fontName = "geogrotesque-regular-"
    constants.fonts = {
        text = findTag(fontName .. "text", tagClasses.font),
        title = findTag(fontName .. "title", tagClasses.font),
        subtitle = findTag(fontName .. "subtitle", tagClasses.font)
    }
end

return constants

end,

["the_flood.network"] = function()
--------------------
-- Module: 'the_flood.network'
--------------------
local network = {}

local function encode(enc, value)
    return string.pack(enc, value):tohex()
end

local function decode(dec, value)
    return string.unpack(dec, value:fromhex())
end

---Generates a waypoint message
---@param x number
---@param y number
---@param z number
---@param type string
---@return string
function network.genWaypoint(x, y, z, type)
    local encodedX = encode("f", x)
    local encodedY = encode("f", y)
    local encodedZ = encode("f", z)
    local message = encodedX .. "," .. encodedY .. "," .. encodedZ .. "," .. type
    return message
end

---Parses a waypoint message
---@param message string
---@return number x, number y, number z, string type
function network.parseWaypoint(message)
    local data = message:split(",")
    local x = data[1]
    local y = data[2]
    local z = data[3]
    local type = data[4]
    local decodedX = decode("f", x)
    local decodedY = decode("f", y)
    local decodedZ = decode("f", z)
    return decodedX, decodedY, decodedZ, type
end

return network

end,

["the_flood.core"] = function()
--------------------
-- Module: 'the_flood.core'
--------------------
-- Lua libraries
local glue = require "glue"

local core = {}

local const = require "the_flood.constants"

---@class vector3D
---@field x number
---@field y number
---@field z number

--- Covert euler into game rotation array, optional rotation matrix, based on this
---[source.](https://www.mecademic.com/en/how-is-orientation-in-space-represented-with-euler-angles)
--- @param yaw number
--- @param pitch number
--- @param roll number
--- @return vector3D, vector3D
function core.eulerToRotation(yaw, pitch, roll)
    local yaw = math.rad(yaw)
    local pitch = math.rad(-pitch) -- Negative pitch due to Sapien handling anticlockwise pitch
    local roll = math.rad(roll)
    local matrix = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}

    -- Roll, Pitch, Yaw = a, b, y
    local cosA = math.cos(roll)
    local sinA = math.sin(roll)
    local cosB = math.cos(pitch)
    local sinB = math.sin(pitch)
    local cosY = math.cos(yaw)
    local sinY = math.sin(yaw)

    matrix[1][1] = cosB * cosY
    matrix[1][2] = -cosB * sinY
    matrix[1][3] = sinB
    matrix[2][1] = cosA * sinY + sinA * sinB * cosY
    matrix[2][2] = cosA * cosY - sinA * sinB * sinY
    matrix[2][3] = -sinA * cosB
    matrix[3][1] = sinA * sinY - cosA * sinB * cosY
    matrix[3][2] = sinA * cosY + cosA * sinB * sinY
    matrix[3][3] = cosA * cosB

    local rollVector = {x = matrix[1][1], y = matrix[2][1], z = matrix[3][1]}
    local yawVector = {x = matrix[1][3], y = matrix[2][3], z = matrix[3][3]}
    return rollVector, yawVector, matrix
end

--- Rotate object into desired angles
---@param objectId number
---@param yaw number
---@param pitch number
---@param roll number
function core.rotateObject(objectId, yaw, pitch, roll)
    local rollVector, yawVector, matrix = core.eulerToRotation(yaw, pitch, roll)
    local object = blam.object(get_object(objectId))
    object.vX = rollVector.x
    object.vY = rollVector.y
    object.vZ = rollVector.z
    object.v2X = yawVector.x
    object.v2Y = yawVector.y
    object.v2Z = yawVector.z
end


function core.secondsToTicks(seconds)
    return 30 * seconds
end

function core.ticksToSeconds(ticks)
    return glue.round(ticks / 30)
end

---@type table<number, {x: number, y: number, z: number}>
local raycastCoords = {}

local currentWaypointsIndexes = {}

function core.deleteWaypoint(index, playerIndex)
    local index = tonumber(index)
    if index then
        local deactivateWaypoint =
            [[(deactivate_nav_point_flag (unit (list_get (players) %s)) waypoint_%s)]]
        execute_script(deactivateWaypoint:format(playerIndex, index))
        raycastCoords[index] = nil
        currentWaypointsIndexes[index] = nil
    end
    return false
end
DeleteWaypoint = core.deleteWaypoint

---Create a waypoint at the given coordinates
---@param x number
---@param y number
---@param z number
---@param type? string
---@param duration? number
---@return boolean
function core.createWaypoint(x, y, z, type, duration)
    for i = 1, 4 do
        if not currentWaypointsIndexes[i] then
            currentWaypointsIndexes[i] = true
            waypointIndex = i
            break
        end
    end
    if waypointIndex then
        if not raycastCoords[waypointIndex] then
            local playerIndex = 0
            local localPlayer = blam.player(get_player())
            if not localPlayer then
                return false
            end
            for i = 0, 15 do
                local player = blam.player(get_player(i))
                if player and player.index ~= localPlayer.index then
                    playerIndex = player.index
                end
            end
            local activateWaypoint = "(activate_nav_point_flag %s (unit (list_get (players) %s)) waypoint_%s 0)"
            local hscCommand = activateWaypoint:format(type or "default", playerIndex, waypointIndex)
            execute_script(hscCommand)
            local scenario = assert(blam.scenario(0), "Unable to get scenario")
            local flags = scenario.cutsceneFlags
            flags[waypointIndex].x = x
            flags[waypointIndex].y = y
            flags[waypointIndex].z = z
            scenario.cutsceneFlags = flags
            raycastCoords[waypointIndex] = {x = x, y = y, z = z}
            set_timer(duration or 4000, "DeleteWaypoint", waypointIndex, playerIndex)
            return true
        end
    end
    return false
end

function core.calculateRaycast(player)
    local rayX = player.x + player.xVel + player.cameraX * const.raycastOffset
    local rayY = player.y + player.yVel + player.cameraY * const.raycastOffset
    -- As we are using biped camera position, we need to add offset to the Z raycast
    -- to make it match view port camera position
    local rayZ = player.z + player.zVel + player.cameraZ * const.raycastOffset + 0.54
    return rayX, rayY, rayZ
end

return core

end,

["the_flood.gameplay_core.healthRegen"] = function()
--------------------
-- Module: 'the_flood.gameplay_core.healthRegen'
--------------------
-- Lua libraries
local const = require "the_flood.constants"

local healthRegen = {}

local isGameClient = function ()
    return blam.isGameDedicated() or blam.isGameHost()
end

--- Attempt to play a sound given tag path and optionally a gain number
function healthRegen.playSound(tagPath, gain)
    local player = blam.player(get_player())
    if player then
        local playSoundCommand = const.hsc.playSound:format(tagPath, player.index, gain or 1.0)
        execute_script(playSoundCommand)
    end
end

--- Regenerate players health on low shield using game ticks
---@param playerIndex? number
function healthRegen.regenerateHealth(playerIndex)
    if blam.isGameSAPP() or isGameClient() then
        local player
        if playerIndex then
            player = blam.biped(get_dynamic_player(playerIndex))
        else
            player = blam.biped(get_dynamic_player())
        end
        if player then
            -- Fix muted audio shield sync
            -- TODO Not sure this will work as expected
            if isGameClient() then
                if player.health <= 0 then
                    player.health = 0.000000001
                end
            end
            if player.health < 1 and player.shield >= 0.98 and blam.isNull(player.vehicleObjectId) then
                local newPlayerHealth = player.health + const.healthRegenAiAmount
                if newPlayerHealth > 1 then
                    player.health = 1
                    if isGameClient() then
                        healthRegen.playSound(const.sounds.humanRifleZoomIn, 5)
                    end
                else
                    player.health = newPlayerHealth
                end
            end
        end
    end
end

return healthRegen
end,

["the_flood.gameplay_core.playerPingObjectives"] = function()
--------------------
-- Module: 'the_flood.gameplay_core.playerPingObjectives'
--------------------
-- Lua libraries
local const = require "the_flood.constants"
local _, harmony
if not blam.isGameSAPP() then
    _, harmony = pcall(require, "mods.harmony")
end
local core = require "the_flood.core"
local network = require "the_flood.network"

local playerPingObjectives = {}

blam.rcon.event("CreateWaypoint", function(message, playerIndex)
    if blam.isGameSAPP() then
        local senderPlayer = blam.player(get_player(playerIndex))
        if senderPlayer then
            for i = 1, 16 do
                local player = blam.player(get_player(i))
                if player then
                    if player.team == senderPlayer.team then
                        blam.rcon.dispatch("CreateWaypoint", message, i)
                    end
                end
            end
        end
    else
        local x, y, z, type = network.parseWaypoint(message)
        core.createWaypoint(x, y, z, type)
    end
end)

---@type number?
local raycastId
local canCreateNewObjective = true
local keyboard_input_address = 0x64C550

function playerPingObjectives.pingObjectives()
    if blam.isGameSAPP() then
        error("This function is not meant to be used on the server")
    end
    if not canCreateNewObjective then
        return
    end
    local player = blam.biped(get_dynamic_player())
    if not player then
        return
    end
    local cKeyPressed = read_byte(keyboard_input_address + 60)
    if blam.isNull(player.vehicleObjectId) then
        if not raycastId then
            local isPlayerOnMenu = read_byte(blam.addressList.gameOnMenus) == 1
            if cKeyPressed > 0 and isPlayerOnMenu then
                local rayX, rayY, rayZ = core.calculateRaycast(player)
                local raycastTagId = const.projectiles.raycastTag.id
                --raycastTagId = blam.findTag("plasma_grenade", tagClasses.projectile).id
                raycastId = spawn_object(raycastTagId, rayX, rayY, rayZ)
                local ray = blam.projectile(get_object(raycastId))
                if ray then
                    ray.xVel = player.cameraX * const.raycastOffset * const.raycastVelocity
                    ray.yVel = player.cameraY * const.raycastOffset * const.raycastVelocity
                    ray.zVel = player.cameraZ * const.raycastOffset * const.raycastVelocity
                    ray.yaw = player.cameraX * const.raycastOffset
                    ray.pitch = player.cameraY * const.raycastOffset
                    ray.roll = player.cameraZ * const.raycastOffset
                end
            end
            return
        end

        local ray = blam.projectile(get_object(raycastId))
        if not ray then
            raycastId = nil
            return
        end
        -- Play the ping sound
        harmony.menu.play_sound(const.sounds.uiFGrenadePath)

        -- Lock the player from creating new objectives
        canCreateNewObjective = false
        AllowCreateNewObjective = function()
            canCreateNewObjective = true
            return false
        end
        set_timer(4000, "AllowCreateNewObjective")

        -- Create the waypoint
        local type = "objective"
        local x = ray.x
        local y = ray.y
        local z = ray.z
        local attachedToId = ray.attachedToObjectId
        if not blam.isNull(attachedToId) then
            local object = blam.object(get_object(attachedToId))
            if object then
                x = x + object.x
                y = y + object.y
                z = z + object.z + 0.45
                if object.class == objectClasses.weapon then
                    type = "weapon"
                end
                if object.class == objectClasses.vehicle then
                    type = "vehicle"
                end
                if object.class == objectClasses.biped then
                    type = "biped"
                end
            end
        end
        blam.rcon.dispatch("CreateWaypoint", network.genWaypoint(x, y, z, type))
    end
end

return playerPingObjectives
end,

----------------------
-- Modules part end --
----------------------
        }
        if files[path] then
            return files[path]
        else
            return origin_seacher(path)
        end
    end
end
---------------------------------------------------------
---------------- Auto Bundled Code Block ----------------
---------------------------------------------------------
api_version = "1.12.0.0"
require "compat53"
require "luna"
blam = require "blam"
local healthRegen

function OnTick()
    for playerIndex = 1, 16 do
        if player_alive(playerIndex) then
            healthRegen.regenerateHealth(playerIndex)
        end
    end
end

function OnMapLoad()
    healthRegen = require "the_flood.gameplay_core.healthRegen"
    require "the_flood.gameplay_core.playerPingObjectives"
    set_callback("tick", "OnTick")
end

function OnRconMessage(playerIndex, message, password)
    return blam.rcon.handle(message, password, playerIndex)
end

function OnScriptLoad()
    set_callback("map load", "OnMapLoad")
    set_callback("rcon message", "OnRconMessage")
    blam.rcon.patch()
end

function OnError()
    console_out(debug.traceback())
end

function OnScriptUnload()
    blam.rcon.unpatch()
end