<?php
	declare(strict_types=1);	
	
	class InvalidValue extends Exception {}
	
	function test1(int $value) {
		echo "Testing exception<br>";
		if ($value <= 100) {
			throw new InvalidValue("MyErrorMessage");
		}
		echo "Correct value $value provided<br>";
	}
	
	$value = 10;  // try 10 and 200
	try {
		test1($value);
	} catch (InvalidValue $t) {
		echo "Error message(T) ".$t."<br>";
	} 
?>