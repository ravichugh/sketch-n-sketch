
//	scripts/generatePosts.sh
//	scripts/generateTutorial.sh
//	scripts/generate.py

// TODO: Include this in front of each html file:
// <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
// <html xmlns="http://www.w3.org/1999/xhtml">

var source = `
pôsts: List (String, String)
posts = nodejs.listdircontent "src/blog/posts"

postheader = nodejs.fileread "src/blog/post-before.html"
`

sns.evaluateEnv({})(source)