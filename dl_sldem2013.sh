#!/bin/bash
parse() {
   #区切り文字列の変更する。
   IFS_SAVE=$IFS
   IFS=/

   #引数に設定する。
   arg="$1"
   set $arg

   #経緯度を設定する
   west="$1";  east="$2"
   south="$3"; north="$4"

   #区切り文字を戻す。
   IFS=$IFS_SAVE
}


makecode(){
   #DTM_MAP_01_N51E350N50E351SC
   prefix="DTM_MAP_01"
   postfix="SC"

   #領域端の数値を入力する。
   local west=$1
   local north=$2
   local east=`expr "$west" + 1`
   local south=`expr "$north" - 1`

   #東端が360の場合、0に変換する
   if [ $east -eq 360 ]; then
      local east="0"
   fi

   #NS符号の記述
   #北端の記述
   if [ $north -ge 0 ]; then
      local north="$(printf "%02d\n" "$north")"
      local north="N${north}"

   elif [ $north -lt 0 ]; then
      local north="${north#-}"
      local north="$(printf "%02d\n" "$north")"
      local north="S$north"

   fi

   #南端の記述
   if [ $south -ge 0 ]; then
      local south="$(printf "%02d\n" "$south")"
      local south="N${south}"

   elif [ $south -le -1 ]; then
      local south="${south#-}"
      local south="$(printf "%02d\n" "$south")"   #
      local south="S$south"

   fi
   
   #E符号の追加
   local west="E$(printf "%03d\n" "$west")"
   local east="E$(printf "%03d\n" "$east")"

   #return変数に書き込む
   return="${prefix}_${north}${west}${south}${east}${postfix}"
}


download_lbl() {
   file="$2.lbl"
   url="${REPOS_ROOT}/$1/data/${file}"
   dir="${DATA_ROOT}"

   if [ -e "$dir/$file" ]; then
      echo "$file: Already exists."
   else
      echo -n "$file: "
      wget --quiet -P $dir $url && echo "Downloaded."
      sleep 1
   fi
}


download_img() {
   #ファイル名の代入
   file="$2.img"
   #URLの生成
   url="${REPOS_ROOT}/$1/data/${file}"
   #
   dir="${DATA_ROOT}"

   if [ -e "$dir/$file" ]; then
      echo "$file: Already exists."
   else
      echo -n "$file: "
      wget --quiet -P $dir $url && echo "Downloaded."
      sleep 1
   fi
}


download_loop() {
   #リストファイルにヘッダー情報を書き込む
   # echo "`expr $east - $west`  `expr $north - $south`" > $code_list

   #西から東へループする。
   for ((i=$west; i<$east; i++)) {
      lon_dir="lon$(printf "%03d\n" "$i")"
      echo $lon_dir

      #北から南へループする。
      for ((j=$north; j>$south; j--)){
         
         #タイルのコードを作成する。
         makecode $i $j

         #lblファイルのダウンロード
         download_lbl $lon_dir $return

         #imgファイルのダウンロード
         download_img $lon_dir $return
      }
   }  
}


pre_execution() {
   #データディレクトリの設定
   while getopts d: OPT
   do
      case $OPT in
         #dの引数有オプションについて、存在する場合にFLG_DをTRUEに設定し、引数をVALUE_Dに代入する
         "d" ) FLG_D="TRUE" ; DATA_ROOT="$OPTARG"
      esac
   done

   # dオプションが存在するか
   if [ ! "$FLG_D" = "TRUE" ]; then
      usage_exit
   fi

   #DATA_ROOTが有効なディレクトリか
   if [ ! -d $DATA_ROOT ]; then
      #存在しない場合
      mkdir $DATA_ROOT
   fi

   #引数の存在チェック
   if [ $# -ne 3 ]; then
      usage_exit
   fi

   #引数のシフト
   shift `expr $OPTIND - 1`

   # 引数の範囲チェック
   parse $1
}


usage_exit() {
   echo "Usage: $0 -d DATA_ROOT_DIR <west>/<east>/<south>/<north>" 1>&2
   exit 1
}


num_evaluation() {
   #引数が数値かどうか判定する。
   num=$1
   if [[ "$num" =~ ^[0-9]+$ ]]; then
     :
   else
      usage_exit
   fi
}


range_check() {
   #経度の範囲をチェックする。
   if [ $west -lt 0 ] || [ $west -ge $east ] || [ $east -gt 360 ]; then
      echo "Invalid longitude value." 1>&2
      echo "   Longitude range is 0 - 360 deg." 1>&2

      #緯度の範囲をチェックする。
      if [ $north -gt 90 ] || [ $south -ge $north ] || [ $south -lt -90 ]; then
         echo "Invalid latitude value." 1>&2
         echo "   Latitude range is -90 - 90 deg." 1>&2
         exit 1
      fi

      exit 1
   fi
}


main() {
   #シェル変数
   REPOS_ROOT="https://data.darts.isas.jaxa.jp/pub/pds3/sln-l-tc-5-sldem2013-v1.0"

   # MAIN
   pre_execution $@

   # parseされた値が数値か判定する
   num_evaluation $west
   num_evaluation $east
   num_evaluation $south
   num_evaluation $north

   # 範囲の確認
   range_check

   # ダウンロードループの実行
   download_loop
}

main $@
