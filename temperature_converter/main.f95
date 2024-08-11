program temperature_converter

implicit none
integer :: choice
real :: temp_input, temp_output

! Display menu
print *, "Temperature Converter"
print *, "1. Celsius to Fahrenheit"
print *, "2. Celsius to Kelvin"
print *, "3. Fahrenheit to Celsius"
print *, "4. Fahrenheit to Kelvin "
print *, "5. Kelvin to Celsius"
print *, "6. Kelvin to Fahrenheit"
print *, "Enter your choice (1-6):"

read *, choice

select case (choice)
    case (1)
        ! Celsius to Fahrenheit
        print *, "Enter temperature in Celsius"
        read *, temp_input
        temp_output = (temp_input * 9.0 / 5.0) + 32.0
        print *, "Temperature in Fahrenheit:", temp_output
    case (2)
        ! Celsius to Kelvin
        print *, "Enter temperature in Celsius"
        read *, temp_input
        temp_output = temp_input + 273.15
        print *, "Temperature in Kelvin:", temp_output
    case (3)
        ! Fahrenheit to Celsius
        print *, "Enter temperature in Fahrenheit"
        read *, temp_input
        temp_output = (temp_input - 32.0) * 5.0 / 9.0
        print *, "Temperature in Celsius:", temp_output
    case (4)
        ! Fahrenheit to Kelvin
        print *, "Enter temperature in Fahrenheit"
        read *, temp_input
        temp_output = ((temp_input - 32.0) * 5.0 / 9.0) + 273.15
        print *, "Temperature in Kelvin:", temp_output
    case (5)
        ! Kelvin to Celsius
        print *, "Enter temperature in Kelvin"
        read *, temp_input
        temp_output = temp_input - 273.15
        print *, "Temperature in Celsius:", temp_output
    case (6)
        ! Kelvin to Fahrenheit
        print *, "Enter temperature in Kelvin"
        read *, temp_input
        temp_output = ((temp_input - 273.15) * 9.0 / 5.0) + 32.0
        print *, "Temperature in Fahrenheit:", temp_output
    case default
        print *, "Invalid choice. Please enter a number between 1 and 6."

    end select

end program temperature_converter
