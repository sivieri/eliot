################################################################################
# Automatically-generated file. Do not edit!
################################################################################

RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include subdir.mk
-include objects.mk

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
ifneq ($(strip $(NC_DEPS)),)
-include $(NC_DEPS)
endif
endif

# Add inputs and outputs from these tool invocations to the build variables 

# All Target
all: eliot_udp.so

# Tool invocations
eliot_udp.so: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: Cross GCC Linker'
	gcc -L/home/gioele/prog/lib/erlang/lib/erl_interface-3.7.6/lib -shared -o "eliot_udp.so" $(OBJS) $(USER_OBJS) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS)$(C_DEPS)$(NC_DEPS)$(LIBRARIES) eliot_udp.so
	-@echo ' '

.PHONY: all clean dependents
.SECONDARY:

-include ../makefile.targets
