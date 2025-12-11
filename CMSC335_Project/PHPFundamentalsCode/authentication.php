<?php
require_once("support.php");

// IMPORTANT: This is just an example and you do not want to have plain passwords like this

$user = "Mary";
$password = "terps";

if(isset($_SERVER['PHP_AUTH_USER']) && isset($_SERVER['PHP_AUTH_PW']) &&
    $_SERVER['PHP_AUTH_USER'] == $user && $_SERVER['PHP_AUTH_PW'] == $password){
	$body = "<h1>Welcome</h1>";
	$page = generatePage($body);
	echo $page;		
} else {
	header("WWW-Authenticate: Basic realm=\"Example System\"");
	header("HTTP/1.0 401 Unauthorized");
}
?>