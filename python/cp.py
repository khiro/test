import urllib2
import functools
from wsgiref import simple_server

CP_HOST = 'http://cp.webhop.net/'
CP_INDEX = 'index.php'
CP_URL = CP_HOST + CP_INDEX
CP_QUERY = '?find=%s&type=&host=&port=&ns=&order=t&sort=a&shch=200'

CP_CSS = 'yp.css'
CP_CPKO = 'img/play_cpko.png'

PEERCAST_HOST = 'bb.isasaka.net'

REPLACE_LIST = [('./', CP_HOST),
                ('localhost', PEERCAST_HOST)]

def not_found(start_response):
  start_response('404 Not Found', [('Content-type', 'text/plain')])
  return '404 Not Found'

def replace_content(data):
  for old, new in REPLACE_LIST:
    data = data.replace(old, new)
  return data

def get_cp_content(start_response, find=""):
  start_response('200 OK', [('Content-type', 'text/html')])
  uri = CP_URL + CP_QUERY % (find)
  print uri
  req = urllib2.Request(uri)
  res = urllib2.urlopen(req, timeout=10)
  return replace_content(res.read())

def response_action(path_info):
  if path_info == '/favicon.ico' or path_info == '/yp.css':
    return not_found
  elif path_info == '/':
    return get_cp_content
  else:
    return functools.partial(get_cp_content, find = path_info[1:])

def application(environ, start_response):
  print environ['PATH_INFO']
  print environ['SCRIPT_NAME'] # if .xxx return 404

  response = response_action(environ['PATH_INFO'])
  return response(start_response)

if __name__ == '__main__':
  server = simple_server.make_server('',8888,application)
  server.serve_forever()
