-- Render the two released pre-meeting survey questions from the SoCO website's aggregate export.
local path = pandoc.path
local event = "soco-interpath-2026-11-20"
local export = "workflows/journal_club/events/2026-11-20_interpath/pre_event_survey/outputs/presentation_export/summary.json"
local topics = {
  {id = "cost_influence", heading = "Cost and coverage",
   choices = {"major", "moderate", "minor", "minimal_if_covered", "unsure_or_outside_role"}},
  {id = "evidence_compelling", heading = "Strength of the evidence",
   choices = {"very", "moderate", "promising_uncertain", "not_compelling", "need_review"}}
}

local function load_summary()
  local cwd = pandoc.system.get_working_directory()
  local roots = {}
  local project = os.getenv("QUARTO_PROJECT_DIR")
  if project and project ~= "" then table.insert(roots, project) end
  for _, input in ipairs(PANDOC_STATE.input_files or {}) do
    if input ~= "-" then table.insert(roots, path.directory(input)) end
  end
  table.insert(roots, cwd)
  local seen = {}
  for _, root in ipairs(roots) do
    if path.is_relative(root) then root = path.join({cwd, root}) end
    root = path.normalize(root)
    while not seen[root] do
      seen[root] = true
      local handle = io.open(path.join({root, export}), "r")
      if handle then
        local raw = handle:read("*a")
        handle:close()
        local ok, data = pcall(pandoc.json.decode, (raw:gsub("^\239\187\191", "")))
        if ok and type(data) == "table" and data.survey_id == event and
            data.data_status == "loaded" and type(data.questions) == "table" then
          return data.questions
        end
        return {}
      end
      root = path.directory(root)
    end
  end
  return {}
end

local function integer(n)
  return type(n) == "number" and n >= 0 and n < math.huge and n == math.floor(n)
end

local function validated(questions, topic)
  local question
  for _, candidate in ipairs(questions) do
    if type(candidate) == "table" and candidate.id == topic.id then
      if question then return nil end
      question = candidate
    end
  end
  if not question or question.status ~= "available" or question.kind ~= "single_choice"
      or not integer(question.n) or question.n == 0 or type(question.cells) ~= "table" then
    return nil
  end

  local cells, total = {}, 0
  local allowed = {not_answered = true}
  for _, value in ipairs(topic.choices) do allowed[value] = true end
  for _, cell in ipairs(question.cells) do
    if type(cell) ~= "table" or type(cell.value) ~= "string" or not allowed[cell.value]
        or cells[cell.value] or type(cell.label) ~= "string" or not cell.label:match("%S")
        or not integer(cell.count) or cell.count > question.n then
      return nil
    end
    cells[cell.value] = cell
    total = total + cell.count
  end
  if total ~= question.n then return nil end
  local ordered = {}
  for _, value in ipairs(topic.choices) do
    if not cells[value] then return nil end
    table.insert(ordered, cells[value])
  end
  if cells.not_answered then table.insert(ordered, cells.not_answered) end
  return {title = question.title, n = question.n, cells = ordered}
end

local function escape(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"):gsub('"', "&quot;"))
end

function Div(div)
  if div.identifier ~= "interpath-perspectives" then return nil end
  local parts, missing = {}, {}
  local questions = load_summary()
  for _, topic in ipairs(topics) do
    local question = validated(questions, topic)
    if question then
      local caption = "Percentages use all " .. question.n .. " respondents in this tally."
      if type(question.title) == "string" and question.title:match("%S") then
        caption = escape(question.title) .. " " .. caption
      end
      table.insert(parts, "<h3>" .. topic.heading .. "</h3><table><caption>" .. caption ..
        '</caption><thead><tr><th scope="col">Response</th><th scope="col">Count</th>' ..
        '<th scope="col">Percent</th></tr></thead><tbody>')
      for _, cell in ipairs(question.cells) do
        table.insert(parts, string.format('<tr><th scope="row">%s</th><td>%d/%d</td><td>%.1f%%</td></tr>',
          escape(cell.label), cell.count, question.n, 100 * cell.count / question.n))
      end
      table.insert(parts, "</tbody></table>")
    else
      table.insert(missing, topic.heading:lower())
    end
  end
  if #missing > 0 then
    local subject = #missing == 2 and "cost and coverage or the strength of the evidence" or missing[1]
    table.insert(parts, '<p class="survey-note">The available survey results do not include ' ..
      'a response breakdown for ' .. subject .. '.</p>')
  end
  return pandoc.RawBlock("html", table.concat(parts, "\n"))
end
