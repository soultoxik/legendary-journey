#!/usr/bin/env python
import urllib
from lxml import html

url = "http://www.ilmanlaatu.fi/mittaaminen/verkot/asemat/asemat.php?nws={}"
xpath = "//*[contains(@name, 'ss')]/optgroup/option"

ss_url = "http://www.ilmanlaatu.fi/mittaaminen/verkot/asemat/asemat.php?nws={}&ss={}"
ss_N = "//*[contains(@class, 'teksti12')]/table/tbody"
ss_E = "//*[contains(@class, 'teksti12')]/table/tbody/tr[4]/td[2]"

def foo(s):
    while True:
        try:
            mock = float(s)
            return s
        except:
            pass
        s = s[:-1]
    return s


with open('./munmap', 'r') as f:
    for line in f:
        sid = line.split(',')[0]

        u = url.format(sid)
        page = html.fromstring(urllib.urlopen(u).read())
        for link in page.xpath(xpath):
            ssid = link.attrib['value']
            ssu = ss_url.format(sid, ssid)
            t = urllib.urlopen(ssu).read()
            nn = t.find('Pohjoiskoordinaatti (&deg;N) </td><td colspan="3">') + len('Pohjoiskoordinaatti (&deg;N) </td><td colspan="3">')
            n = foo(t[nn:nn+8])
            e = foo(t[nn+76:nn+84])
            print ssid, n, e

