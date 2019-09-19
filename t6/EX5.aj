package br.ufsm.lpbd.banking.aspect;

import br.ufsm.lpbd.banking.core.OverdraftAccount;
import br.ufsm.lpbd.banking.core.Account;

/* Aspecto que adiciona 1% do valor da transação sempre que um 
 * empréstimo for realizado (conta overdraft -> conta corrente). 
 */

public aspect EX5 {
	// A lógica aqui é que sempre quando houver um debit na conta overdraft, significa que houve um empréstimo, ou seja, 
	// a conta foi transferida da overdraft para a corrente/poupança.
	pointcut addTax(OverdraftAccount acc, float quant) : execution(* Account.debit(float)) && args(quant) && target(acc);
	
	after(OverdraftAccount acc, float quant) : addTax(acc, quant) {
		float tax = (float) (quant * 0.01);
		acc.registerTax(tax);
		System.out.println("> (EX5) Taxa de " + tax + " (1%) para a conta:\n" +
									   "     - Conta: " + acc.getAccountNumber());
	}
}