#!/usr/bin/env python3

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import subprocess
import os.path

def sanitize_name(name):
    r = ''
    for c in name:
        if c.isalpha():
            r += c
        else:
            r += '_'
    return r

driver = webdriver.Firefox()

output = subprocess.check_output(['find', 'build', '-name', '*.html']).decode('utf8')
for i, p in enumerate(output.splitlines()):
    n = os.path.relpath(p, 'build')
    url_dev = f'http://localhost:3000/{n}'
    url_ref = f'https://docs.wire.com/{n}'
    img_basename = sanitize_name(n) + '_' + str(i)

    try:
        print(f'./screenshots/{i:03}-{img_basename}_dev.png')
        driver.get(url_dev)
        driver.get_full_page_screenshot_as_file(f'./screenshots/{i:03}-{img_basename}_dev.png')
        print(url_ref)
        driver.get(url_ref)
        driver.get_full_page_screenshot_as_file(f'./screenshots/{i:03}-{img_basename}_ref.png')
    except:
        pass

driver.close()



# assert "Python" in driver.title
# elem = driver.find_element(By.NAME, "q")
# elem.clear()
# elem.send_keys("pycon")
# elem.send_keys(Keys.RETURN)
# assert "No results found." not in driver.page_source
#
