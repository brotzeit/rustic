-- Filter to convert rustdoc to org mode
function tablelength(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end
function table.shallow_copy(t)
  local t2 = {}
  for k,v in pairs(t) do
    t2[k] = v
  end
  return t2
end
function dump(o)
  if type(o) == 'table' then
    local s = '{ '
    for k,v in pairs(o) do
      if type(k) ~= 'number' then k = '"'..k..'"' end
      s = s .. '['..k..'] = ' .. dump(v) .. ','
    end
    return s .. '} '
  else
    return tostring(o)
  end
end


Span = function(el)
  if tablelength(el.content) >= 3 and el.content[1]["text"] == "Expand" and el.content[3]["text"] == "description" then
    return {}
  elseif el.classes:includes("since") or el.classes:includes("inner") or tablelength(el.content) == 1 then
    return {}
  elseif tablelength(el.content) == 0 then
    return {}
  elseif tablelength(el.content) == 1 then
    return el.content
  end
end
Image = function(el)
  return {}
end

cleanblocks = {
  Str = function(el)
    if el.text == "[src]" then
      return pandoc.Str(" ")
    end
  end,
Link = function(el)
  return pandoc.Span(el.content)
end,

Header = function(el)
  if el.classes:includes("section-header") then
    return {}
  end

  if not el.content then
    return {}
  end

  if el.classes:includes("small-section-header") and el.content and tablelength(el.content) > 0 then
    return pandoc.Header(1, pandoc.List({el.content[1]}))
  end

  if el.classes:includes("impl") and el.content then
    return pandoc.Header(2, pandoc.List({el.content[1]}))
  end

  if el.classes:includes("fqn") and el.level == 1 and el.content and el.content[1].content then
    crate = ""
    for i,v in ipairs(el.content[1].content) do
      if v.content and v.content[1].text then
        crate = crate .. v.content[1].text .. "::"
      end
    end

    return pandoc.Header(1, el.content)
  end
  if el.classes:includes("hidden") then
    return pandoc.Plain(el.content) -- We hide the headlines from search results by making them plain. Maybe the solution can be nicer, need to think about it.
    -- return pandoc.Header(4, el.content)
  end


  if el.classes:includes("method") then
    local code = el.content[1]
    local methodname = ""
    local must_use_text = ""
    local contains_must_use = false
    local in_methodname = true
    local in_must_use_text = false

    if string.match(code.text, "must_use") then
      in_methodname = false
      contains_must_use = true
    end

    for i = 1, #code.text do
      local c = code.text:sub(i, i);

      if in_methodname then
        methodname = methodname .. c
      elseif in_must_use_text then
        must_use_text = must_use_text .. c
      end

      if c == "]" then
        in_methodname = true
      elseif c == "\"" then
        in_must_use_text = true
      end
    end

    if contains_must_use then
      return pandoc.List({pandoc.Header(3, pandoc.List({pandoc.Code(methodname)})), pandoc.Plain(must_use_text:sub(1, -3))})
    end
    return pandoc.Header(3, pandoc.List({pandoc.Code(methodname)}))
  end

  return pandoc.Header(el.level - 1, el.content)
end,

Div = function(el)
  if tablelength(el.content) == 1 then -- Removes one layer of unnecessary nesting.
    return el.content
  end
  if el.classes:includes("shortcuts") or el.classes:includes("sidebar-elems") or el.classes:includes("theme-picker") or el.classes:includes("infos") or el.classes:includes("search-container")  or el.classes:includes("sidebar-menu") or el.classes:includes("logo-container") or el.classes:includes("toggle-wrapper") then
    return {}
  elseif el.classes:includes("variant") and el.classes:includes("small-section-header") and el.content[1] and tablelength(el.content[1].content) > 1 then
    return pandoc.List({pandoc.Header(2, pandoc.Code(el.content[1].content[2].text))})
    else
    return pandoc.Div(el.content)
  end
end,

Plain = function(el)
  for i,v in ipairs(el.content) do
    if v.t == "Span" and v.content[1] and v.content[1].t == "Str" and v.content[1].text == "Run" then
      return {}
    end

    if v.t == "Span" and (v.classes:includes("loading-content") or tablelength(v.content) == 0) and tablelength(el.content) == 1 then --bug here! 1 week later: Why did I not explain what the bug was? I have no idea now.
      return {}
    end

    if v.t == "Span" and v.classes:includes("emoji") then
      table.remove(el.content, 1)
      return pandoc.Plain(el.content)
      end
  end
end,

CodeBlock = function(el)
  if el.classes:includes("line-numbers") then
    return {}
  else
    return pandoc.Para(pandoc.Str("#+BEGIN_SRC rust \n" .. el.text .. "\n#+END_SRC"))
  end
end,

Para = function(el)
  if el.content[1] and el.content[1].t == "Span" and tablelength(el.content[1].content) == 0 then
    return {}
  end
end,

}

function Pandoc(el)
  return pandoc.Pandoc(pandoc.walk_block(pandoc.Div(el.blocks), cleanblocks), pandoc.Meta({}))
end
