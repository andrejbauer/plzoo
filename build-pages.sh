#!/bin/bash

# The location of the plzoo master branch repo
MASTER=../plzoo

LANGS=$(find ${MASTER}/src -type d -depth 1 -exec basename {} \;)

# All the generated files are in the language subfolder
mkdir -p language

# Copy README files to language.markdown files
for lang in ${LANGS}
do
    if [ -r "${MASTER}/src/${lang}/README.markdown" ]
    then        
        echo "---" > "language/${lang}.md"
        echo "layout: language" >> "language/${lang}.md"
        echo "language: ${lang}" >> "language/${lang}.md"
        echo "---" >> "language/${lang}.md"
        cat "${MASTER}/src/$lang/README.markdown" >> "language/${lang}.md"
        git add "language/${lang}.md"
    elif [ -r "language/${lang}.md" ]
    then
        # The language is gone
        git rm "language/${lang.md}"
    fi
done

# Generate the index file
echo "" > "_includes/language-list.md"
for lang in ${LANGS}
do
    if [ -r "language/${lang}.md" ]
    then
        echo "[${lang}](language/${lang}.html)" >> "_includes/language-list.md"
        echo -n ": " >> "_includes/language-list.md"
        if [ -r "${MASTER}/src/${lang}/tagline.md" ]
        then
            cat "${MASTER}/src/${lang}/tagline.md" >> "_includes/language-list.md"
        else
            echo "cool language" >> "_includes/language-list.md"
        fi
            echo "" >> "_includes/language-list.md"
    fi
done
git add "_includes/language-list.md"

