find ./ -iname "*.md" -type f -exec sh -c 'pandoc "${0}" -o "converted/${0%.md}.docx"' {} \;
