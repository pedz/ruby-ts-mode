#!/usr/bin/env ruby

# dog When writing indent rules, you can use ‘treesit-check-indent’ to
# check if your indentation is correct. To debug what went wrong, set
# ‘treesit--indent-verbose’ to non-nil. Then when you indent, Emacs
# tells you which rule is applied in the echo area.

fred = 2
dog = while fred < 12
        puts 'hi'
        fred *= 4
        fred
      end
       puts dog.inspect


dog = while fred > 12 do
        fred *= 4
      end

a + b *
    c *
    d +
12

call_me("today",
        "tomorrow")

array = [
  9.9,
  10,
  11,
]

new_hash = {
  long_ugly_name:  9,
  frog:    12,
  daft:           92,
  egg:            99
}

hazy = 99 if europe != asia
hazy /= 12 while hazy > 99

while dog < fred
  dog += 12
end

until fred > dog do
  fred *= 1.2
  next if friday
end

for value in [1, 2, 3] do
  redo if ambivalent
  puts value
  break
end

class HaveNoClass
end

class MyClass < HaveNoClass
  ConstAssign = 12

  def sample(arg2, *args, **rest, &proc)
    puts "12"
  end

  # dog has flees
  def blob(arg12,
           arg2,                # bob
           xxx,
           yyy
          )

    case life
    when "hard",
         "shop",
         "hot"
      play("victim")
    when /abcd/,
         /efgh/
      buy("twitter",
"friday"
         )
    end

    happy = 99
    if god != satin
      sin += 12
      egypt = 92
    elsif
      dog = 99
    else
      golf -= 18
    end
    unless bismark
      sink += 12
    end

    i = 12
    while i < 44
      i += 2
    end
    puts i

  end
end
