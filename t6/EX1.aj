package br.ufsm.lpbd.banking.aspect;

import br.ufsm.lpbd.banking.core.Account;

// Notifica o adm sempre que um saque maior que R$ 10.000,00 for realizado.
public aspect EX1 {
	pointcut notificaSaqueAlto(Account acc, float quant) : call(* Account.debit(float)) && args(quant) && target(acc);
	
	before(Account acc, float quant): notificaSaqueAlto(acc, quant) {
		if (quant > 10000) {
			System.out.println("!! (EX1) NOTIFICAÇÃO DE SAQUE > 10.000:\n" + 
		                                   "    >> A conta " + acc.getAccountNumber() + " sacou " + quant);
		}
	}
}