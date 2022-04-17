# -*- coding: utf-8 -*-
"""
Created on Fri Apr 15 12:58:44 2022

@author: camila.ochoa
"""

list_=[1,2,3,4,5,6]


def func (list_):
    i=0
    while i< len(list_) and list_[i]==0:
        i:i+1
    
    a=1
    while i< len(list_) and list_[i]==1:
        i=i+1
        
    return i==len(list_) and 2*a==i

respuesta= func(list_)


import math

def primo(p):
    for i in range(2, p):
        if p % i == 0:
            return False
        else:
            return True
    

def funcion (p,q,primo):
    gcd=0
    if(q==0):
        print ("el valor de q es cero")
    
    elif primo== True:
        print("El número es primo")  
        
    
    else:
        gcd= math.gcd(p,q)
        
    valorp= p/gcd
    valorq= q/gcd
    valor_total="{0}/{1}".format(valorp,valorq)
    
    return valor_total

fraccion= funcion(10,2,primo(10))
    
    
    
    
valor= funcion(10,2)    

