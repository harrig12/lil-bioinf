hugo
cd public && 
sed -i -e '/1 January, 0001 at 00:00 UTC/d' index.html && 
git add --all && 
git commit -m "Publishing to gh-pages" && 
git push &&
cd ..
