#!/usr/bin/env python
import urllib
from lxml import html

url = "http://www.ilmanlaatu.fi/mittaaminen/verkot/asemat/asemat.php?nws={}"
xpath = "//*[contains(@name, 'ss')]/optgroup/option"

ss_url = "http://www.ilmanlaatu.fi/mittaaminen/verkot/asemat/asemat.php?nws={}&ss={}"
ss_N = '//*[contains(@class, "teksti12")]/table/tbody/tr[3]/td[2]'
ss_E = '//*[contains(@class, "teksti12")]/table/tbody/tr[4]/td[2]'

sid_ss = {}

with open('./munmap', 'r') as f:
    for line in f:
        sid = line.split(',')[0]

        u = url.format(sid)
        page = html.fromstring(urllib.urlopen(u).read())
        sid_ss[sid] = []
        for link in page.xpath(xpath):
            ssid = link.attrib['value']

            ssu = ss_url.format(sid, ssid)
            sspage = html.fromstring(urllib.urlopen(ssu).read())
            for i in sspage.xpath('//*[contains(@class, "teksti12")]/table/tbody'):
                print i
            #e = sspage.xpath(ss_E)
            #sid_ss[sid].append([link.attrib['value'], n, e])

print sid_ss
