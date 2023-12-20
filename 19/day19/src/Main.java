import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static java.lang.Math.min;
import static java.lang.Math.max;

class Rule {
    public final Optional<Character> operand;

    public final Optional<Character> operator;

    public final Optional<Integer> threshold;

    public final String target;

    public Rule(String pattern) {
        if (!pattern.contains(":")) {
            this.operand = Optional.empty();
            this.operator = Optional.empty();
            this.threshold = Optional.empty();
            this.target = pattern;

            return;
        }

        List<String> components = List.of(pattern.split(":"));
        this.target = components.get(1); // rhs is the target state

        // further split the LHS which looks like x<dddd
        String lhs = components.get(0);
        this.operand = Optional.of(lhs.charAt(0));
        this.operator = Optional.of(lhs.charAt(1));
        this.threshold = Optional.of(Integer.parseInt(lhs.substring(2)));
    }
}

class Workflow {

    public final String name;
    public final List<Rule> rules;

    public Workflow(String pattern) {
        Pattern workflowpattern = Pattern.compile("^(?<name>[a-zA-Z]+)\\{(?<rules>[^}]+)}$");
        var matcher = workflowpattern.matcher(pattern);

        if (matcher.matches()) {
            this.name = matcher.group("name");
            this.rules = Arrays.stream(matcher.group("rules").split(",")).map(Rule::new).toList();
        } else {
            throw new RuntimeException("bad pattern");
        }
    }

    public String execute(Map<Character, Integer> rating) {
        for (Rule rule : this.rules) {
            if (rule.operand.isEmpty()) {
                return rule.target;
            }

            if (rating.containsKey(rule.operand.get())) {
                int ratingvalue = rating.get(rule.operand.get());

                if (rule.operator.orElseThrow() == '>' && ratingvalue > rule.threshold.orElseThrow()) {
                    return rule.target;
                }

                if (rule.operator.orElseThrow() == '<' && ratingvalue < rule.threshold.orElseThrow()) {
                    return rule.target;
                }
            }
        }

        throw new RuntimeException("bad state: no rules match");
    }
}

// Press Shift twice to open the Search Everywhere dialog and type `show whitespaces`,
// then press Enter. You can now see whitespace characters in your code.
public class Main {

    private static Map<Character, Integer> parseRating(String rating) {
        String[] ratings = rating.substring(1, rating.length() - 1).split(",");

        return Arrays.stream(ratings)
                .collect(Collectors.toMap(
                        r -> r.charAt(0),
                        r -> Integer.parseInt(r.substring(2)),
                        // Merge function for handling duplicates (if any)
                        (existing, replacement) -> { throw new IllegalStateException("Duplicate key"); },
                        HashMap::new
                ));
    }

    public static boolean executeWorkflows(Map<String, Workflow> workflows, Map<Character, Integer> rating) {
        String target = "in";

        while (!target.equals("A") && !target.equals("R")) {
            Workflow flow = workflows.get(target);
            target = flow.execute(rating);
        }

        return target.equals("A");
    }

    // 616133947947000
    // 167409079868000
    public static long calccombinations(Map<String, Workflow> workflows, String target, long xmin, long xmax, long mmin, long mmax, long amin, long amax, long smin, long smax) {

        if (target.equals("A")) {
            return max(0, xmax - xmin + 1) * max(0, mmax - mmin + 1) * max(0, amax - amin + 1) * max(0, smax - smin + 1);
        }

        if (target.equals("R")) {
            return 0;
        }

        Workflow flow = workflows.get(target);

        long combinations = 0;

        for (Rule rule : flow.rules) {
            if (rule.operand.isEmpty()) {
                combinations += calccombinations(workflows, rule.target, xmin, xmax, mmin, mmax, amin, amax, smin, smax);
                break;
            }

            char operand = rule.operand.get();
            char operator = rule.operator.orElseThrow();
            int threshold = rule.threshold.orElseThrow();

            if (operand == 'x') {
                long newxmin = operator == '>' ? max(xmin, threshold + 1) : xmin;
                long newxmax = operator == '<' ? min(xmax, threshold - 1) : xmax;

                combinations += calccombinations(workflows, rule.target, newxmin, newxmax, mmin, mmax, amin, amax, smin, smax);

                xmax = operator == '>' ? min(xmax, threshold) : xmax;
                xmin = operator == '<' ? max(xmin, threshold) : xmin;
            } else if (operand == 'm') {
                long newmmin = operator == '>' ? max(mmin, threshold + 1) : mmin;
                long newmmax = operator == '<' ? min(mmax, threshold - 1) : mmax;

                combinations += calccombinations(workflows, rule.target, xmin, xmax, newmmin, newmmax, amin, amax, smin, smax);

                mmax = operator == '>' ? min(mmax, threshold) : mmax;
                mmin = operator == '<' ? max(mmin, threshold) : mmin;
            } else if (operand == 'a') {
                long newamin = operator == '>' ? max(amin, threshold + 1) : amin;
                long newamax = operator == '<' ? min(amax, threshold - 1) : amax;

                combinations += calccombinations(workflows, rule.target, xmin, xmax, mmin, mmax, newamin, newamax, smin, smax);

                amax = operator == '>' ? min(amax, threshold) : amax;
                amin = operator == '<' ? max(amin, threshold) : amin;
            } else if (operand == 's') {
                long newsmin = operator == '>' ? max(smin, threshold + 1) : smin;
                long newsmax = operator == '<' ? min(smax, threshold - 1) : smax;

                combinations += calccombinations(workflows, rule.target, xmin, xmax, mmin, mmax, amin, amax, newsmin, newsmax);

                smax = operator == '>' ? min(smax, threshold) : smax;
                smin = operator == '<' ? max(smin, threshold) : smin;
            }
        }

        return combinations;
    }

    public static void main(String[] args) {
        List<String> workflowLines = new ArrayList<>();
        List<String> ratingLines = new ArrayList<>();

        Scanner stdin = new Scanner(System.in);

        while (stdin.hasNextLine()) {
            String line = stdin.nextLine();

            if (line.equals("")) {
                break;
            }

            workflowLines.add(line);
        }

        while (stdin.hasNextLine()) {
            String line = stdin.nextLine();
            ratingLines.add(line);
        }

        Map<String, Workflow> workflows = workflowLines.stream().map(Workflow::new).collect(Collectors.toMap(
                w -> w.name,
                w -> w,
                (existing, replacement) -> { throw new RuntimeException("cannot have duplicates"); },
                HashMap::new
        ));

        List<Map<Character, Integer>> ratings = ratingLines.stream().map(Main::parseRating).toList();

        int sum = ratings.stream()
                .filter(r -> executeWorkflows(workflows, r))
                .map(r -> r.values().stream().reduce(Integer::sum).orElseThrow())
                .reduce(Integer::sum)
                .orElseThrow();

        System.out.println("part 1: " + sum);

        long combinations = calccombinations(workflows, "in", 1, 4000, 1, 4000, 1, 4000, 1, 4000);
        System.out.println("part 2: " + combinations);
    }


}