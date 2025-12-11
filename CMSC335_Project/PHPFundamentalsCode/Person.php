<?php
	declare(strict_types=1);
	
	/**
	 * Person class (example of DocBlock)
	 * @author CMSC
	 */
	
	class Person {
		private $name;
		protected $address;
	
		public function __construct(string $name, string $address) {
			$this->name = $name;		// Notice this->name (no $ for name)
			$this->address = $address;  // We need $this-> to refer to data members
		}
	
		public function __toString() {
			return "<b>Name:</b> ".$this->name.", <b>Address:</b> ".$this->address;		
		}

		public function getName() : string {
			return $this->name;
		}
	
		public function getAddress() : string {
			return $this->address;
		}
	
		public function __destruct() {
			echo "Person __destruct called<br />";
		}
		
		public final function getCode() {
			return 12345;
		}
		
		public function getInfo() {
			echo "Class Information: ", get_class($this);
		}
	}
?>