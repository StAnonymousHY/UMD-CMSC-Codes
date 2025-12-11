<?php
	declare(strict_types=1);	
	require("Person.php");

	/**
	 * Student class
	 * @author CMSC
	 */
	class Student extends Person {
		private $gpa;
		public static $MAXGPA = 4.0;
	
		public function __construct(string $name, string $address, float $gpa) {
			parent::__construct($name, $address);
			$this->gpa = $gpa;
		}
	
		public function __toString() {
			$personStr = parent::__toString();
			return $personStr.", <b>Gpa: ".$this->gpa.", MAXGPA: ".self::$MAXGPA."</b>"; 				
		}
		
		public function getGpa() {
			return $this->gpa;
		}
		
		public static function printMAXGPA() {
			echo "MAXGPA value: ", self::$MAXGPA;   /* Notice use of self */
		}
		
		/* Cannot override final method
		public final function getCode() {
			return 54321;
		}*/
	}
?>