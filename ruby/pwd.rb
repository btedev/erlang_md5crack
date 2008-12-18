require 'digest/md5'
include Digest

class Pwd  
  def self.encrypt(s)
    MD5.hexdigest(s)
  end
  
  def self.decrypt(hashed, length)    
    analyzed = 0
    test = "a" * length
    max = "z" * length
    found = nil

    while test <= max  
      if MD5.hexdigest(test) == hashed      
        found = test
        break
      end
      analyzed += 1
      puts "analyzed: #{analyzed}" if analyzed % 500000 == 0

      test.next! 
    end
    found
  end  
end