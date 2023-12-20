import sys
import itertools

def flipflop(name: str, inputs: list[str], outputs: list[str]):
    def pulser(pulse: bool, _from: str):
        if pulse:
            return []
        pulser.state = not pulser.state
        return [ (name, o, pulser.state) for o in outputs ]

    pulser.state = False

    return pulser

def broadcaster(name: str, inputs: list[str], outputs: list[str]):
    def pulser(pulse: bool, _from: str):
        return [(name, o, pulse) for o in outputs]

    return pulser

def conjunctor(name: str, inputs: list[str], outputs: list[str]):
    def pulser(pulse: bool, _from: str):
        pulser.state[_from] = pulse

        if all([ v for k, v in pulser.state.items() ]):
            return [(name, o, False) for o in outputs]
        else:
            return [(name, o, True) for o in outputs ]

    pulser.state = { input: False for input in inputs }
    
    return pulser

def parse(line):
    [definition, outputs] = line.strip().split(' -> ')

    if definition == 'broadcaster':
        t = 'broadcaster'
        name = 'broadcaster'
    elif definition[0] == '%':
        t = 'flipflop'
        name = definition[1:]
    elif definition[0] == '&':
        t = 'conjunctor'
        name = definition[1:]
    else:
        raise Exception(f"bad input: {line} (no type for `{definition}`)")

    outputs = outputs.split(', ')

    return t, name, outputs

def makemodule(moduletype, name, inputs, outputs):
    if moduletype == 'broadcaster':
        return broadcaster(name, inputs, outputs)
    if moduletype == 'flipflop':
        return flipflop(name, inputs, outputs)
    if moduletype == 'conjunctor':
        return conjunctor(name, inputs, outputs)
    
    raise Exception("bad argument moduletype: " + moduletype)

def handlepulse(modules, pulses: list[tuple[str, str, bool]]):
    responses = []
    for (fromname, toname, pulse) in pulses:
        module = modules.get(toname)

        if module:
            responses += module(pulse, fromname)
            # print(f"{fromname} -{'high' if pulse else 'low'}-> {toname}\t {toname} state: {getattr(module, 'state', None)}")
        else:
            # print(f"[warn] wanted to send pulse ({pulse}) from {fromname} to non-existent module {toname}")
            pass

    if responses:
        tail = handlepulse(modules, responses)
    else:
        tail = []

    return responses + tail

def turns_on(recipient, pulse):
    return recipient == 'rx' and pulse == False and part2 == None

if __name__ == "__main__":
    moduleinput = [parse(line) for line in sys.stdin]

    # store for each module what its inputs are (necessary for conjunctor)
    inputs: dict[str, list[str]] = dict()
    for (t, n, outs) in moduleinput:
        for o in outs:
            inputs[o] = inputs.get(o, []) + [n]

    modules = { name: makemodule(t, name, inputs.get(name, []), outputs) for (t, name, outputs) in moduleinput }

    allpulses = { True: 0, False: 0 }

    for i in range(1, 1001):
        allpulses[False] += 1
        pulses = handlepulse(modules, [('button', 'broadcaster', False)])

        for (f, to, pulse) in pulses:
            allpulses[pulse] += 1

    print(f"part 1: {allpulses[True] * allpulses[False]}")

    # nomnoml graph:
    # for name, ins in inputs.items():
    #     for i in ins:
    #         print(f"[{i}] -> [{name}]")

    modules = { name: makemodule(t, name, inputs.get(name, []), outputs) for (t, name, outputs) in moduleinput }

    # rx has 1 input: qn. qn has 4 inputs. keep track of when these inputs are True (maybe it's periodic?)
    qniters = dict()
    periods = dict()
    for i in itertools.count(1):
        pulses = handlepulse(modules, [('button', 'broadcaster', False)])

        for (sender, recipient, pulse) in pulses:
            if recipient == 'qn' and pulse:
                qniters[sender] = qniters.get(sender, []) + [i]
                if len(qniters[sender]) > 1:
                    periodmaybe = qniters[sender][-1] - qniters[sender][-2]
                    print(f"period {sender}: {periodmaybe}")
                    periods[sender] = periodmaybe
        if len(periods) == 4:
            from math import lcm
            print(lcm(*[ v for k, v in periods.items() ]))
            break
