<?php

$target_dir = "tmp/";
$output_file = "tmp/output.html";
$runner_file = "tmp/runner.sml";
$target_file = "tmp/input.sml";
$uploadOk = 1;

$smlFileType = strtolower(pathinfo(basename($_FILES["fileToUpload"]["name"]),PATHINFO_EXTENSION));

// Check if file already exists
if (file_exists($target_file)) {
    echo "Sorry, file already exists.";
    $uploadOk = 0;
}
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
    if (move_uploaded_file($_FILES["fileToUpload"]["tmp_name"], $target_file)) {
        echo "Checking ". basename( $_FILES["fileToUpload"]["name"]). " for style errors. Errors are marked in red below.";
	exec("python mkrunner.py " . $target_file);
	exec("sml parse.sml ast-skeleton.sml " . $runner_file . " fail.sml");
	exec("rm " . $runner_file);
	exec("rm " . $target_file);
	$myfile = fopen($output_file, "r") or die("Something went wrong.");
	echo fread($myfile,filesize($output_file));
	fclose($myfile);
	exec("rm " . $output_file);
    } else {
      	if(file_exists($_FILES["fileToUpload"]["tmp_name"])) {
	   echo "Odd, file exists but won't move.";
	   $myfile = fopen($_FILES["fileToUpload"]["tmp_name"], "r") or die("Something went wrong.");
	   echo fread($myfile, filesize($_FILES["fileToUpload"]["tmp_name"]));
	}
        echo "Sorry, there was an error uploading your file.";
    }
}
?>
