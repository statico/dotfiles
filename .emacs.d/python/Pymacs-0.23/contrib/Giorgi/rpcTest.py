
import xmlrpclib,os

if __name__ == "__main__":
    print "My dir is:"+os.getcwd()
    print "Connecting...."
    s = xmlrpclib.Server('http://127.0.0.1:9000')
    print "Sending message..."
    s.message("hi emacs, I am remoted hosted")
    print "Opening directory:"
    s.find_file(os.getcwd())
    s.switch_to_buffer("extensions")
    print "Finding python stuff"
    s.occur(r'\.py')
    print "Closing Connection"
    s.closeConnection()

