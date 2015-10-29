#!/bin/sh

a="$1"
if [ $a == "jar" ];then
    mvn clean
    mvn assembly:assembly
    java -jar target/scala-pl-1.0-jar-with-dependencies.jar
elif [ $a == "scala" ];then
    mvn clean
    mvn scala:run -Dlauncher=hm #or mvn scala:run
else
    mvn scala:run -DmainClass=hm.Core
fi

