       Identification Division.
       Program-id.          Lab2.
       
      *      Name: Tyler Zysberg. Description: LAB2. Asks user for
      *      input of investment amount and calculates balance

       Data division.
       Working-storage section.
       
       01  BegAmount       PIC S9(8)V99999 value -1.
       01  AmountLeft      PIC 9(8)v99999.
       
       01  Interest        pic 9(8)v99.
       01  InterestRate    PIC S99v9999 value -1.
       01  TotalInt        PIC 9(8)V99999 value 0.
       01  TotalIntForm    PIC $$$$$$$.$$.
       
       01  Months          PIC S9(3) value -1.
       01  counter         Pic 9(3) value 1.
       
       01  FinalAmt        PIC 9(8)v99999.
       
       01  c-format        pic ZZZ.
       01  l-format        pic $$$$,$$$,$$9.99.
       01  i-format        pic $$$$$$$.$$.
       01  End-format      PIC ZZ.ZZZ.
       
       
       Procedure Division.
       000-main.
           Perform until BegAmount > 0
           Display "Enter Investment Amount: " with no advancing
           accept BegAmount
           if BegAmount <= 0
              display "Must be positive number"
           end-if
           end-perform
           Move BegAmount to AmountLeft
           
           
           
           Perform until InterestRate > 0
           Display "Enter Annual Interest Rate: " with no advancing
           accept InterestRate
           if InterestRate <= 0
               Display "Annual Interest Rate must be positive"
           end-if
           end-perform
               
           Perform until Months > 0
           Display "Enter Number of Months: " with no advancing
           accept Months
           if Months <= 0
              display "Must be positive number"
            end-if
           end-perform
           
           Display "Investment Schedule: "
           Display "  "
           
           
            display "Month   Beg Balance       Interest     Additional"
            Perform until counter > months
            if counter > 1
                 Compute AmountLeft Rounded = AmountLeft + Interest
             end-if
            Compute Interest Rounded = (.01 * InterestRate / 12) 
			    * AmountLeft
            Compute TotalInt = TotalInt + Interest
            Compute FinalAmt = AmountLeft + Interest
            
            
            
            move counter to c-format
            move AmountLeft to l-format
            move interest to i-format
            display c-format l-format "     "  i-format
            add 1 to counter
            end-perform
            
            Display "Balance Summary: "
            Move Months to c-format
            Move BegAmount to l-format
            Move InterestRate to End-format
            Move TotalInt to TotalIntForm
            
            
            Display "  Investment Amount     " l-format
            Display "  Interest Rate                 " End-format
			   "%"
            Display "  Months                            " c-format
            Display "  Total Interest             " TotalIntForm
            Move FinalAmt to l-format
            Display "  Final Balance         " l-format
            
           Stop run.
        
           