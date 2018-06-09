<?php

$target_dir = "tmp/";
$output_file = "tmp/output.html";
$runner_file = "tmp/runner.sml";
$uploadOk = 1;

$smlFileType = strtolower(pathinfo(basename($_FILES["fileToUpload"]["name"]),PATHINFO_EXTENSION));

$target_file = $_FILES["fileToUpload"]["tmp_name"];

// Check if file already exists
//if (file_exists($target_file)) {
//    echo "Sorry, file already exists.";
//    $uploadOk = 0;
//}
// Check file size
if ($_FILES["fileToUpload"]["size"] > 500000) {
    echo "Sorry, your file is too large.";
    $uploadOk = 0;
}
// Allow certain file formats
if($smlFileType != "sml" ) {
    echo "Sorry, only SML files are allowed.";
    $uploadOk = 0;
}
// Check if $uploadOk is set to 0 by an error
if ($uploadOk == 0) {
    echo "Sorry, your file was not uploaded.";
// if everything is ok, try to upload file
} else {
        echo "Checking ". basename( $_FILES["fileToUpload"]["name"]). " for style errors. Errors are marked in red below.";
	exec("/bin/sml fail.sml", $aaa);
	print_r($aaa);
	exec("mv " . $target_file . " " . "aaa.sml");
	$target_file = "aaa.sml";
	exec("ls", $aaa);
	print_r($aaa);
	exec("echo \"val _ = markAppends \"aaa.sml\"\" > runner.sml");
	exec("ls", $aaa);
	print_r($aaa);
	$runner_file = "runner.sml";
	if (file_exists($runner_file)) {
	   echo "runner exists";
	}
	exec("sml parse.sml ast-skeleton.sml " . $runner_file . " fail.sml", $aaa);
	print_r($aaa);
	exec("rm " . $runner_file);
	exec("ls tmp", $aaa);
	print_r($aaa);
	$myfile = fopen($output_file, "r") or die("Something went wrong.");
	echo fread($myfile,filesize($output_file));
	fclose($myfile);
	exec("rm " . $output_file);
    }
?>
