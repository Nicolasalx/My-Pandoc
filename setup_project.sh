read -p $'\e[1mNew project name: \e[0m' PROJECT_NAME

stack new $PROJECT_NAME
cp -r ./$PROJECT_NAME/* .
cp -r ./$PROJECT_NAME/.* .
rm -fr ./$PROJECT_NAME

echo -e "resolver: lts-20.11\npackages:\n- ." > stack.yaml

rm LICENSE
rm README.md
rm CHANGELOG.md

echo -e "\n$PROJECT_NAME" >> .gitignore

sed -i "s/a.out/$PROJECT_NAME/g" Makefile
