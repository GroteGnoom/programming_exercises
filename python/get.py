import urllib2
from HTMLParser import HTMLParser

html = urllib2.urlopen("https://google.com/search?q=banaan").read()

class MyHTMLParser(HTMLParser):
	def __init__(self):
		HTMLParser.__init__(self)
		self.inscript = False
	def handle_starttag(self, tag, attrs):
		#print "Start:", tag, attrs
		if tag == "script":
			self.inscript = True
	def handle_endtag(self, tag):
		#print "End:", tag
		if tag == "script":
			self.inscript = False
	def handle_data(self, data):
		if self.inscript == False : 
			if data.strip() != "": 
				print "data:", data

parser = MyHTMLParser()
parser.feed(html)
