#!/usr/bin/python
# -*- coding: utf-8 -*-

import re
import sys
import urllib
import urllib2
import xml.etree.ElementTree as etree
import socket
import struct
import cookielib
import argparse
from HTMLParser import HTMLParser

class NicoLive:

  LOGIN_URL = 'https://secure.nicovideo.jp/secure/login'
  ANTENA_URL = LOGIN_URL + '?site=nicolive_antenna'
  ALERTSTATUS_URL = 'http://live.nicovideo.jp/api/getalertstatus'

  GETPLAYSTATUS_URL = 'http://watch.live.nicovideo.jp/api/getplayerstatus?v=%s'
  THREAD_TEMPLATE = '<thread thread="%s" version="20061206" res_from="-1"/>'
  MY_URL = 'http://live.nicovideo.jp/my'

  def __init__(self):
    self.login_status = False
    self.thread = None
    self.addr = None
    self.port = None

  def login(self, params):
    cookie = cookielib.CookieJar()
    opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cookie))
    urllib2.install_opener(opener)
    res = self._urlopen(self.LOGIN_URL, params)
    if cookie._cookies['.nicovideo.jp']:
      self.login_status = True

  def watch(self, lv, cb):
    if self.login_status is False:
      print 'you haven\'t login'
      exit(0)
    lv_url = self.GETPLAYSTATUS_URL % lv
    dom = self._urlopen(lv_url, None, etree.fromstring)
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

  def _urlopen(self, url, params=None, f=lambda x: x):
    request = urllib2.Request(url)
    if params != None:
      postdata = urllib.urlencode(params)
      res = urllib2.urlopen(request, postdata)
    else:
      res = urllib2.urlopen(request)
    data = res.read()
    return f(data)

  def antena(self, params):
    dom = self._urlopen(self.ANTENA_URL, params, etree.fromstring)
    ticket = ''
    for item in dom.findall('.//'):
      if item.tag == 'ticket':
        ticket = item.text
    dom = self._urlopen(self.ALERTSTATUS_URL, {'ticket':ticket}, etree.fromstring)
    community_id_list = []
    addr = None
    port = None
    thread = None
    for item in dom.findall('.//'):
      if item.tag == 'community_id':
        community_id_list.append(item.text)
      elif item.tag == 'addr':
        addr = item.text
      elif item.tag == 'port':
        port = item.text
      elif item.tag == 'thread':
        thread = item.text
    print addr, port
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((addr, int(port)))
    msg =  self.THREAD_TEMPLATE % (thread)
    while True:
      sock.send(msg)
      sock.send(struct.pack('b',0))
      data = sock.recv(1024)

  def my(self):
    if self.login_status is False:
      print 'you haven\'t login'
      exit(0)
    data = self._urlopen(self.MY_URL)
    myparser = MyHTMLParser()
    myparser.feed(data)
    favorite_lv_set = set(myparser.lv_list)
    if len(favorite_lv_set) == 0:
      print 'no favorite community\'s live'
    else:
      for lv in favorite_lv_set:
        print '%s : %s' % (lv[0], lv[1])

class MyHTMLParser(HTMLParser):

  WATCH_RE = re.compile('http://live.nicovideo.jp/watch/(lv[0-9]+)', re.I)

  def __init__(self):
    HTMLParser.__init__(self)
    self.lv_list = []

  def handle_starttag(self, tag, attrs):
    if tag == 'a' and attrs:
      if len(attrs) > 1 and attrs[0][0] == 'href':
        watch_re = self.WATCH_RE.search(attrs[0][1])
        if watch_re:
          self.lv_list.append((watch_re.group(1), attrs[1][1]))

def main():
  def _print_msg(msg):
    print msg
  parser = argparse.ArgumentParser(description='NicoLive Comment View')
  parser.add_argument('-lv', type=str, help='target nicolive lv')
  parser.add_argument('-my', action='store_true', help='show favorite community\'s live')
  args = parser.parse_args()

  params = {'mail':'YOUR@MAILADDRESS', 'password':'YOURPASSWORD'}
  nicolive = NicoLive()
  nicolive.login(params)

  if args.my:
    #nicolive.antena(params)
    nicolive.my()
  elif args.lv:
    nicolive.watch(args.lv, _print_msg)
  else:
    print parser.print_help()
  
if __name__ == "__main__":
  main()
