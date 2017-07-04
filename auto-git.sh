git add .
read -p "Commit description, default is 'Auto commit': " desc

if [ -z "$desc" ]; then
    desc="Auto commit"
fi

git commit -m "$desc"
git push origin master
