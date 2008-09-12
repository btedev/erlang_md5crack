require 'pwd'

f = File.new("results.txt","w")

(3..6).each do |len|
  10.times do   
    plain = ""
    len.times { |x| plain << (97 + rand(26)) }
    puts "plaintext word is: #{plain}"

    before = Time.now
    decrypted = Pwd.decrypt(Pwd.encrypt(plain), len)  
    after = Time.now
    elapsed = (after - before) * 1000
    puts "length #{len} took #{elapsed} ms"
    f << "#{plain},#{len},#{elapsed}\n"  
  end
end

