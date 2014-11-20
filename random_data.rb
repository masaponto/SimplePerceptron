def fix_num(n)
  return n == 0 ? -1 : 1
end

random = Random.new

s = "["
s += random.rand(-10..10).to_s + "," + random.rand(-10..10).to_s + "," + random.rand(-10..10).to_s
s += "]\n"

s += random.rand(-10.0..10.0).to_s + "\n"

s += "0\n"

s += "["
20.times do
  s += " ( 0" + ", [1," + random.rand(-9..-1).to_s + ", " + random.rand(0..9).to_s + "]),"
  s += " ( 1" + ", [1," + random.rand(0..9).to_s + ", " + random.rand(-9..-1).to_s + "]),"
end

s = s.chop
s += "]\n"

s += "rand.png"

File.write("rand.dat", s)
