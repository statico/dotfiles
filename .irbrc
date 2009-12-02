# vim:ft=ruby:

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:AUTO_INDENT] = true

def ri(*names)
    system(%{ri #{names.map {|name| name.to_s}.join(" ")}})
end

