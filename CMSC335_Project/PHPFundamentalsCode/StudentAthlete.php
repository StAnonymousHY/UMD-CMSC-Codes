<?php
	declare(strict_types=1);
	require("Student.php");
	require("Athlete.php");
	
	/**
	 * StudentAthlete class
	 * @author CMSC
	 */
	class StudentAthlete extends Student implements Athlete {
		static $totalAthletes = 0;
		private $sport;
	
		public function __construct(string $name, string $address, float $gpa,
									string $sport) {
			parent::__construct($name, $address, $gpa);
			$this->sport = $sport;
			StudentAthlete::$totalAthletes++;
		}
	
		public function getSport() {
			return $this->sport;
		}
	
		public function __toString() {
			$studentStr = parent::__toString();
			return $studentStr.", <b>Sport: ".$this->sport."</b>";
		}
		
		public static function maxSalary() {
			return "50000";
		}
	}
?>