### Note
```html
data:text/html,<textarea style="width:100%;height:99.9%"></textarea>
```

### Justify
```javascript
javascript:document.body.style.setProperty('text-align', document.body.style.textAlign === 'justify' ? 'unset' : 'justify');
```

### [Sci-hubify](https://old.reddit.com/r/programming/comments/garx20/scihub_now_a_browser_extension_that_gives_you/fp2mlvy/)
```javascript
javascript:location.href="///sci-hub.tw/"+location.href;
```

### [Search](https://www.online-tech-tips.com/cool-websites/the-12-best-bookmarklets-every-browser-should-have/)
```javascript
javascript:var [n,q]=prompt('', '-').split('-'); n !== "" ? location.href="https://duckduckgo.com/?q=site%3A" + window.location.hostname + window.location.pathname.split('/', 1 + Number(n)).join('/') + ' ' + escape(q) : undefined
```
