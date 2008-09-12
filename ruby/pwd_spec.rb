require 'pwd'

describe Pwd do

  it "should encrypt a string as an MD5 digest" do
    Pwd.encrypt("hi").should eql("49f68a5c8493ec2c0bf489821c21fc3b")
  end

  it "should crack the MD% hash of a lowercase string" do
    Pwd.decrypt("49f68a5c8493ec2c0bf489821c21fc3b",2).should eql("hi")
  end

end