<?php
	require_once("support.php");

	if (isset($_POST["locatePerson"])) {
		// notice we are passing a parameter via get method
		header("Location: locate.php?searchType=person");
	} elseif (isset($_POST["locateLab"])) {
		// notice we are passing a parameter via get method
		header("Location: locate.php?searchType=lab");
	} elseif (isset($_POST["generalInformation"])) {
		header("Location: http://www.umd.edu/");
	} else {
		$body = <<<EOBODY
			<h1>Welcome to UMD</h1>
			<form action="{$_SERVER["PHP_SELF"]}" method="post">
				<input type="submit" name="locatePerson" value="Locate Person" />
				<input type="submit" name="locateLab" value="Locate Lab" />
				<input type="submit" name="generalInformation" value="General Information" /><br>
			</form>		
EOBODY;
	}
	
	$page = generatePage($body);
	echo $page;
?>