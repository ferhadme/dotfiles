#!/usr/bin/bash

# Author: Farhad Mehdizada
# Simple script I use for compiling and running maven-archetype-quickstart apps
# Usage without arguments:
#   ./run-maven-archetype-quickstart.sh
# Usage with arguments:
#   ./run-maven-archetype-quickstart.sh target/java-example-1.0-SNAPSHOT.jar com.farhad.App


mvn package

bold=$(tput bold)
normal=$(tput sgr0)

echo -e "\e[1;0m[\e[1;34mINFO\e[1;0m] \e[1;0m${bold}------------------------------------------------------------------------"
echo -e "\e[1;0m[\e[1;34mINFO\e[1;0m] \e[1;32m${bold}OUTPUT"
echo -e "\e[1;0m[\e[1;34mINFO\e[1;0m] \e[1;0m${bold}------------------------------------------------------------------------${normal}"

# for custom path
JAR_PATH="$1"
CLASS_PATH="$2"

if [[ ${JAR_PATH} == "" ]] && [[ ${CLASS_PATH} == "" ]]
then
    GROUP_ID=$(mvn help:evaluate -Dexpression=project.groupId -q -DforceStdout)
    ARTIFACT_ID=$(mvn help:evaluate -Dexpression=project.artifactId -q -DforceStdout)
    VERSION=$(mvn help:evaluate -Dexpression=project.version -q -DforceStdout)    
    java -cp target/${ARTIFACT_ID}-${VERSION}.jar ${GROUP_ID}.App
else
    java -cp ${JAR_PATH} ${CLASS_PATH}
fi

