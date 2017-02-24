function NumberLines(element, nr) {
	var content = "";

	for (i = 1; i <= nr; i++) {
		content = content + "<span class=\"clsLineNumber\">" + ("0" + i).slice(-2) + "|</span>\n";
	}

	element.innerHTML = content;
}
