#!/bin/bash

#此文件供参考，实际未使用

base_path=$1

if [ $# != 1 ];then
  echo "参数数量不正确"
	exit 1
fi

if [ ! -f "${base_path}/download/list.cache" ]; then
  echo "歌曲列表文件未找到"
  exit 1
fi

cat ${base_path}/download/list.cache | while read line
do
  row=${line// /}
  arr=(${row//:/ })
  if [ ${#arr[@]} = 2 ]; then
    song=${arr[0]}
    name="${arr[1]}"
    wget -q -O "${base_path}/download/${name}.mp3" http://music.163.com/song/media/outer/url?id=${song}.mp3
  fi
done

rm -f ${base_path}/download/list.cache
echo "ok"