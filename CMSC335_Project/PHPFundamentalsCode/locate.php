<?php
	require_once("support.php");

	$body = "";
	if (isset($_GET["searchType"])) {
		switch ($_GET["searchType"]) {
			case "person":
				$body = "<strong>Searching for person</strong>";
				break;
			case "lab":
				$body = "<strong>Searching for lab</strong>";
				break;
		}	
	}
	
	$page = generatePage($body);
	echo $page;
?>