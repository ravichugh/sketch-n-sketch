.load ../all-junk/w3color.js
names = getColorArr("names")
for (i in names) {
  name = names[i]
  color = w3color(name)
  rgb = '(' + color.red + ', ' + color.green + ', ' + color.blue + ')'
  hsl = '(' + color.hue + ', ' + color.sat + ', ' + color.lightness + ')'
  console.log('  ("' + name + '", (' + rgb + ', ' + hsl + '))')
}
