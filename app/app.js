Bacon.EventStream.prototype.ajax = function() {
  return this["switch"](function(params) { return Bacon.fromPromise(jQuery.ajax(params)) });
}
let $ = (selector) => document.querySelector(selector)

function inputE(element) {
  return Bacon.fromEvent(element, "keyup").map(_ => element.value);
}

function inputB(element) {
  return inputE(element).toProperty(element.value);
}

let queryTermB = inputB($("#query"))
let queryE = Bacon.fromEvent($("#go"), "click")

let imagesB = queryTermB.sampledBy(queryE)
  .flatMap(q => Bacon.fromPromise(jQuery.ajax("https://duplicates.introitu.info/similar/5?url=" + q)))
  .toProperty([]);

let output = $(".result ul")
imagesB.map(urls => urls.map(url => "<li>" + '<img src="' + url + '" /></li>').join())
  .onValue(html => result.innerHTML = html)
