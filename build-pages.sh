#!/bin/bash

REPOSRC="https://github.com/andrejbauer/plzoo/tree/master/src/"

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
        echo "Found ${lang}"
        echo "---" > "language/${lang}.markdown"
        echo "layout: language" >> "language/${lang}.markdown"
        echo "language: ${lang}" >> "language/${lang}.markdown"
        echo "---" >> "language/${lang}.markdown"
        echo "" >> "language/${lang}.markdown"
        cat "${MASTER}/src/$lang/README.markdown" >> "language/${lang}.markdown"
        git add "language/${lang}.markdown"
    fi
done

# Generate the index file
echo "" > "_includes/language-list.markdown"
for lang in ${LANGS}
do
    if [ -r "language/${lang}.markdown" ]
    then
        echo "[${lang}](language/${lang}.html)" >> "_includes/language-list.markdown"
        echo -n ": " >> "_includes/language-list.markdown"
        if [ -r "${MASTER}/src/${lang}/tagline.markdown" ]
        then
            cat "${MASTER}/src/${lang}/tagline.markdown" >> "_includes/language-list.markdown"
        else
            echo "a very cool language without description" >> "_includes/language-list.markdown"
        fi
            echo "" >> "_includes/language-list.markdown"
    fi
done
git add "_includes/language-list.markdown"
