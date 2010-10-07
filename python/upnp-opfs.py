import sys
import socket
import select
import time
import re
import urllib2
import urllib
import xml.dom.minidom

# ref: http://docs.python.org/library/xml.dom.minidom.html
def getText(nodelist):
  rc = []
  for node in nodelist:
    if node.nodeType == node.TEXT_NODE:
      rc.append(node.data)
  return ''.join(rc)

class UPnPError(Exception):

  def __init__(self, reason, log_message=None, *args):
    self.reason = reason
    self.log_message = log_message
    self.args = args

  def __str__(self):
    message = "UPnP %s:" % (self.reason)
    if self.log_message:
      sys.stderr.write(message + self.log_message)
      for arg in self.args:
        print arg
    else:
      sys.stderr.write(message)

class MSearchClient(object):

  sock_buf = 8192
  ssdp_host = ('239.255.255.250', 1900)

  m_search_request = 'M-SEARCH * HTTP/1.1\r\n' + \
      'HOST: 239.255.255.250:1900\r\n' + \
      'MAN: "ssdp:discover"\r\n' + \
      'MX: 3\r\n' + \
      'ST: urn:schemas-upnp-org:%s:%s:1\r\n' +\
      '\r\n'
  location_re = re.compile('location:\s(http:\/\/.+)', re.I)

  def __init__(self, timeout):
    self.timeout = timeout

  def __call__(self, id, service):
    sock = self.create_sock()
    sock.sendto(self.m_search_request % (id, service), self.ssdp_host)
    return self.recv_data(sock)

  def create_sock(self):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.settimeout(self.timeout)
    return sock

  def recv_data(self, sock):
    while True:
      try:
        recv_data = sock.recv(self.sock_buf)
        re_result = self.location_re.search(recv_data)
        if re_result:
          return self.get_url(re_result.group(1))
      except socket.timeout:
        sys.stderr.write('timeout\n')
        break
      except (KeyboardInterrupt, SystemExit):
        break
    return False

  def get_url(self, location):
    req = urllib2.Request(location)
    res = urllib2.urlopen(req, timeout=self.timeout)
    dom = xml.dom.minidom.parseString(res.read())
    return getText(dom.getElementsByTagName('URLBase')[0].childNodes)

class SOAPClient(object):
  action_template = '<?xml version="1.0"?>' +\
      '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/"' +\
      's:encodingStyle="http://schemas.xmlsoap.org/encoding/">'+\
      '<s:Body>' +\
      '<m:%s xmlns:m="urn:schemas-upnp-org:service:%s:1">' +\
      '%s' +\
      '</s:Body>' +\
      '</s:Envelope>' +\
      '\r\n'

  def __init__(self, url, service, timeout):
    self.url = url
    self.service = service
    self.timeout = timeout

  def __call__(self, command, body):
    data = self.action_template % (command, self.service, body)
    req = urllib2.Request('%s/%s' % (self.url, self.service), data)
    req.add_header('SoapAction', '%s#%s' % (self.service, command))
    res = urllib2.urlopen(req, timeout=self.timeout)
    return res.read()

  def __str__(self):
    return '%s : %s' % (self.url, self.service)

class UPnPClient(object):

  def __init__(self):
    self.timeout = 5
    self.soap_client = None

  def msearch(self):
    m_search_services = [('service', 'WANIPConnection'),
                         ('device', 'InternetGatewayDevice'),
                         ('service', 'WANPPPConnection')]
    msearch_client = MSearchClient(timeout=self.timeout)
    for id, service in m_search_services:
      try:
        url = msearch_client(id, service)
      except urllib2.HTTPError, e:
        message = 'msearch failure %s : %s' % (id, service)
        raise UPnPError('urllib2.HTTPerror', message, e)
      if url:
        self.soap_client = SOAPClient(url, service, self.timeout)
        return True
    return False

  def open_port(self, external_port, internal_port, proto, ip, description):
    command = 'AddPortMapping'
    body_template = '<NewRemoteHost></NewRemoteHost>' +\
        '<NewExternalPort>%s</NewExternalPort>' +\
        '<NewProtocol>%s</NewProtocol>' +\
        '<NewInternalPort>%s</NewInternalPort>' +\
        '<NewInternalClient>%s</NewInternalClient>'+\
        '<NewEnabled>1</NewEnabled>' +\
        '<NewPortMappingDescription>%s</NewPortMappingDescription>' +\
        '<NewLeaseDuration>0</NewLeaseDuration>' +\
        '</m:AddPortMapping>'
    body = body_template % (external_port, proto, internal_port, ip, description)
    data = self.send_soap_request(command, body)
    return True

  def close_port(self, external_port, proto):
    command = 'DeletePortMapping'
    body_template = '<NewRemoteHost></NewRemoteHost>' +\
        '<NewExternalPort>%s</NewExternalPort>' +\
        '<NewProtocol>%s</NewProtocol>' +\
        '</m:DeletePortMapping>'
    body = body_template % (external_port, proto)
    data = self.send_soap_request(command, body)
    return True

  def port_mapping_entry(self, index):
    command = 'GetGenericPortMappingEntry'
    body = '<NewPortMappingIndex>%s' % index +\
    '</NewPortMappingIndex>' +\
    '</m:GetGenericPortMappingEntry>'
    data = self.send_soap_request(command, body)
    dom = xml.dom.minidom.parseString(data)
    return [getText(dom.getElementsByTagName(tag)[0].childNodes) for tag in ['NewExternalPort',  'NewInternalPort', 'NewProtocol', 'NewInternalClient', 'NewPortMappingDescription']]
  
  def get_wan_ip(self):
    command = 'GetExternalIPAddress'
    body = '</m:GetExternalIPAddress>'
    data = self.send_soap_request(command, body)
    dom = xml.dom.minidom.parseString(data)
    return getText(dom.getElementsByTagName('NewExternalIPAddress')[0].childNodes)

  def send_soap_request(self, command, body):
    self.assert_soap_client()
    try:
      return self.soap_client(command, body)
    except urllib2.HTTPError, e:
      message = 'command failure %s : %s : %s ' % (command, body, self.soap_client)
      raise UPnPError('urllib2.HTTPerror', message, e)

  def assert_soap_client(self):
    if self.soap_client is None and self.msearch() is False:
      raise UPnPError('Router not found')


def main():
  def port_check(client, external_port, then, els):
    mapping_list = []
    for index in range(0,10):
      try:
        mapping_list.append(client.port_mapping_entry(index))
      except:
        break

    if (filter(lambda x: x[0] == external_port, mapping_list)):
      print then
    else:
      print els

  client = UPnPClient()
  print client.get_wan_ip()
  ip = socket.gethostbyname(socket.gethostname())
  external_port = '48888'
  internal_port = '8888'
  proto = 'TCP'
  description = 'test port'
  client.open_port(external_port, internal_port, proto, ip, description)
  port_check(client, external_port, 'open port success', 'open port failure')

  client.close_port(external_port, proto)  
  port_check(client, external_port, 'close port failure', 'close port success')

if __name__ == "__main__":
  main()
