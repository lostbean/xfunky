function round(number, decimal)
    local multiplier = 10^(decimal or 0)
    return math.floor(number * multiplier + 0.5) / multiplier
  end

function hbar(pct, fgcolor, bgcolor, x_dimension, y_dimension, maxpct)
    local gdbar_diff  = maxpct / x_dimension
    local gdbar_div   = pct / gdbar_diff
    local gdbar_rdiv  = round(gdbar_div)
    return("^fg(" .. fgcolor .. ")^r(" .. math.floor(gdbar_rdiv) .. "x" .. y_dimension .. ")^fg(" .. bgcolor .. ")^r(" .. math.floor(x_dimension - gdbar_rdiv) .. "x" .. y_dimension .. ")^fg()")
  end


function vbar(pct, fgcolor, bgcolor, x_dimension, y_dimension, maxpct)
    local gdbar_diff  = maxpct / y_dimension
    local gdbar_div   = pct / gdbar_diff
    local gdbar_rdiv  = round(gdbar_div)
    return ("^ib(1)^fg(".. bgcolor .. ")^r(" .. x_dimension .. "x" .. y_dimension .. ")^fg(" .. fgcolor .. ")^r(" .. x_dimension .. "x" .. math.floor(gdbar_rdiv) .. "-" .. x_dimension .. "+" .. math.floor((y_dimension - gdbar_rdiv) / 2) .. ")^ib(0)^fg()")
  end

function conky_cpuvbar(core)
    return vbar(conky_parse("${cpu " .. core .."}"), "white", "darkgrey", 5, 12, 100)
end

function conky_h100bar(metric)
    return hbar(conky_parse("${" .. metric .. "}"), "white", "darkgrey", 25, 8, 100)
end

function conky_wirelessQualityBar(interface)
  local cmd = "iwlist " .. interface .. " scan | grep Quality | grep -o '[-0-9]*' | xargs | awk '{ print $1/$2}'"
  local quality = conky_parse('${exec ' .. cmd .. '}')
  return vbar(quality, "white", "darkgrey", 5, 12, 1)
end