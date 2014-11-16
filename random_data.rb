def fix_num(n)
  return n == 0 ? -1 : 1
end

s = "["
random = Random.new
20.times do
  s += " ( -1" + ", [1," + random.rand(-9..0).to_s + ", " + random.rand(-9..1).to_s + "]),"
  s += " ( 1" + ", [1," + random.rand(0..9).to_s + ", " + random.rand(0..9).to_s + "]),"
end

s = s.chop
s += "]"
File.write("rand.dat", s)
