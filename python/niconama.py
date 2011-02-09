#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import urllib
import urllib2
import xml.etree.ElementTree as etree
import socket
import struct
import cookielib
import argparse

class NicoLive:

  LOGIN_URL = 'https://secure.nicovideo.jp/secure/login'
  GETPLAYSTATUS_URL = 'http://watch.live.nicovideo.jp/api/getplayerstatus?v=%s'
  THREAD_TEMPLATE = '<thread thread="%s" version="20061206" res_from="-1"/>'

  def __init__(self):
    self.login_status = False
    self.thread = None
    self.addr = None
    self.port = None


  def login(self, params):
    cookie = cookielib.CookieJar()
    opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cookie))
    urllib2.install_opener(opener)
    postdata = urllib.urlencode(params)
    request = urllib2.Request(self.LOGIN_URL, postdata)
    res = urllib2.urlopen(request)
    if cookie._cookies['.nicovideo.jp']:
      self.login_status = True

  def watch(self, lv, cb):
    if self.login_status is not True:
      print 'you haven\'t login'
      exit(0)

    lv_url = self.GETPLAYSTATUS_URL % lv
    request = urllib2.Request(lv_url)
    res = urllib2.urlopen(request)
    data = res.read()
    dom = etree.fromstring(data)
    status = dom.findall('.')[0].attrib
    if status['status'] == 'fail':
      print 'program was finished or login failed'
      exit(0)

    for item in dom.findall('.//'):
      if item.tag == 'thread':
        self.thread = item.text
      elif item.tag == 'addr':
        self.addr = item.text
      elif item.tag == 'port':
        self.port = item.text

    self._get_msg(cb)

  def _get_msg(self, cb):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((self.addr, int(self.port)))
    msg =  self.THREAD_TEMPLATE % (self.thread)
    while True:
      sock.send(msg)
      sock.send(struct.pack('b',0))
      try:
        data = sock.recv(1024)
        cb(data)
      except(KeyboardInterrupt, IOError), e:
        print e
        exit(0)
      except Exception:
        print 'unhandled exception'
        exit(0)

def main():
  def _print_msg(msg):
    print msg
  parser = argparse.ArgumentParser(description='NicoLive Comment View')
  parser.add_argument('-lv', type=str,
                      help='target nicolive lv')
  args = parser.parse_args()
  if args.lv:
    params = {'mail':'YOUR@MAILADDRESS', 'password':'YOURPASSWORD'}
    nicolive = NicoLive()
    nicolive.login(params)
    nicolive.watch(args.lv, _print_msg)
  else:
    print parser.print_help()
  
if __name__ == "__main__":
  main()
