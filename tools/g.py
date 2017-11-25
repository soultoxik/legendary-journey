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
    i = 0
    for line in f:
        i += 1
        sid = line.split(',')[0]

        u = url.format(sid)
        page = html.fromstring(urllib.urlopen(u).read())
        for link in page.xpath(xpath):
            ssid = link.attrib['value']
            print sid, ssid
